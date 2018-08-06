{-# LANGUAGE OverloadedStrings #-}

module Emit where 

import qualified Intermediate as IR
import TypeCheck (isNum)
import qualified Grammar as G ( Program, Type(..) )
import Grammar (OP(..), Ident, IdentList,
    VarDecl, TypeDecl, CallByRef, ToDownTo)

import Control.Monad.Except hiding (void)
import Control.Applicative

import Utils (p')
import Paskell as P

import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.AST.Global
import LLVM.Context
import LLVM.Module
import LLVM.AST.ParameterAttribute
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short hiding (length)
import qualified ConvertIR as Conv
import qualified Intermediate as IR
import Codegen



toShortBS =  toShort . BS.pack 
toString = BS.unpack . fromShort
name' = Name . toShortBS

toLLVMType t = 
    case t of G.TYint  -> int
              G.TYbool -> bool
              G.TYreal -> double
              G.Void   -> void

toParamList params = map mapParam params
    where mapParam (x,t,byref) = 
            Parameter (toLLVMType t) (name' x) (if byref then [Dereferenceable 4] else [])
-------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- LLVM-IR and LLVM-AST given IR
codegen :: AST.Module -> IR.Program -> IO (AST.Module, String)
codegen mod pr = withContext $ \context ->
    liftIO $ withModuleFromAST context newast $ \m -> 
    do llstr <- moduleLLVMAssembly m
       return (newast, BS.unpack llstr)
    where newast = runLLVM mod (genProgram pr)

-- LLVM-IR given parse tree
printllvm :: G.Program -> IO String
printllvm ast = let ir = Conv.convProgram ast in
    do (llvmast, llstr) <- codegen (emptyModule "MainModule") ir
       return llstr

-------------------------

genDecl :: IR.Decl -> Codegen ()
genDecl d@(IR.DeclVar _ _) = genDeclVar d
-- genDecl d@(IR.DeclFunc x args retty blk _) = genDeclFunc d

genDeclFunc :: IR.Decl -> LLVM ()
genDeclFunc (IR.DeclFunc x args retty blk _) = do
    define (toLLVMType retty) (toShortBS x) (toSig args) body
    where 
        toSig xs = map (\(a,b,c) -> (toLLVMType b, name' a)) xs
        body = do
            entry' <- addBlock (toShortBS "entry")
            setBlock entry'
            forM args $ \(i,t,_) -> do
                var <- alloca (toLLVMType t)
                store var (local (toLLVMType t) (name' i))
                assign (toShortBS i) var
            defs <- genBlock blk
            -- return dummy variable value
            getvar (toShortBS x) (toLLVMType retty) >>= load >>= ret
            return defs

genDeclProc :: IR.Decl -> LLVM ()
genDeclProc (IR.DeclProc x args blk _) = do
    define (toLLVMType retty) (toShortBS x) (toSig args) body
    where 
        retty = G.Void
        toSig xs = map (\(a,b,c) -> (toLLVMType b, name' a)) xs
        body = do
            entry' <- addBlock (toShortBS "entry")
            setBlock entry'
            forM args $ \(i,t,_) -> do
                var <- alloca (toLLVMType t)
                store var (local (toLLVMType t) (name' i))
                assign (toShortBS i) var
            defs <- genBlock blk
            retvoid
            return defs

genDeclVar :: IR.Decl -> Codegen ()
genDeclVar (IR.DeclVar xs _) = do
    forM xs $ \(i,t) -> do
        var <- alloca (toLLVMType t)
        assign (toShortBS i) var
    return ()


genDeclGlob :: IR.Decl -> LLVM ()
genDeclGlob d@(IR.DeclVar _ _) = genDeclVarGlob d
genDeclGlob d@(IR.DeclProc _ _ _ _) = genDeclProc d 
genDeclGlob d@(IR.DeclFunc _ _ _ _ _) = genDeclFunc d 


genDeclVarGlob :: IR.Decl -> LLVM ()
genDeclVarGlob (IR.DeclVar xs _) = do
    forM xs $ \(i,t) -> do
        gvar (toLLVMType t) (name' i)
    return ()


-- generate entry point main()
genMain :: IR.Statement -> LLVM ()
genMain s = genDeclProc (IR.DeclProc "main" args (IR.Block [] s G.Void) G.Void) 
    where args = []

genProgram :: IR.Program -> LLVM ()
genProgram (IR.Program p (IR.Block ds s _) _) = do
    forM ds genDeclGlob
    genMain s


genBlock :: IR.Block -> Codegen [Definition]
genBlock (IR.Block ds s _) = do
    forM ds genDecl
    genStatement s


genStatement :: IR.Statement -> Codegen [Definition]
genStatement (IR.StatementEmpty) = return []
genStatement (IR.StatementSeq xs _) = (forM xs genStatement) >>= (return.concat)
genStatement (IR.Assignment (IR.Designator x _ xt) expr _) = do
    (rhs, defs) <- genExpr expr
    var <- getvar (toShortBS x) (toLLVMType xt)
    store var rhs
    return defs

genStatement (IR.StatementIf expr s1 ms2 _) = do 
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    (cond, defs1) <- genExpr expr
    cbr cond ifthen ifelse

    -- if.then
    setBlock ifthen
    defs2 <- genStatement s1
    br ifexit
    getBlock

    -- if.else
    setBlock ifelse
    defs3 <- genStatement s2
    br ifexit
    getBlock

    -- if.exit
    setBlock ifexit
    return $ defs1 ++ defs2 ++ defs3
    where s2 = case ms2 of 
                Nothing -> IR.StatementEmpty
                Just x  -> x

genStatement (IR.StatementFor x expr1 todownto expr2 s _) = do
    ftest <- addBlock "for.test"
    fbody <- addBlock "for.body"
    fstep <- addBlock "for.step"
    fexit <- addBlock "for.exit"

    defs1 <- genStatement (IR.Assignment loopvar expr1 G.Void) 
    br ftest

    -- for.test
    setBlock ftest
    (cond, defs2) <- genExpr (IR.Relation varfactor optest expr2 G.TYbool) 
    cbr cond fbody fexit

    -- for.body
    setBlock fbody
    defs3 <- genStatement s
    br fstep

    -- for.step
    setBlock fstep
    -- todo implment Char stepping
    defs4 <- genStatement (IR.Assignment loopvar step G.Void) 
    br ftest

    -- for.exit
    setBlock fexit
    return $ defs1 ++ defs2 ++ defs3 ++ defs4
    where loopvar = IR.Designator x [] (IR.getType expr1)
          varfactor = IR.FactorDesig loopvar (IR.getType expr1)
          (optest, opstep) = if todownto then (OPle, OPplus) else (OPge, OPminus)
          step = IR.Add varfactor opstep (IR.FactorInt 1 G.TYint) (IR.getType expr1)


genStatement (IR.StatementWhile expr s _) = do
    wtest  <- addBlock "while.test"
    wbody <- addBlock "while.body"
    wexit <- addBlock "while.exit"
    br wtest

    -- entry
    setBlock wtest
    (cond, defs1) <- genExpr expr
    _ <- cbr cond wbody wexit

    -- body
    _ <- setBlock wbody
    defs2 <- genStatement s
    br wtest

    -- exit
    _ <- setBlock wexit
    return $ defs1 ++ defs2 

genStatement (IR.ProcCall f xs t) = do
    (args, defs) <- mapM genExpr xs >>= (return.unzip)
    call (externf (toLLVMType t) (name' f)) args
    return $ concat defs


-- returns %x for final expression value, and stores any intermediate instructions in the block
genExpr :: IR.Expr -> Codegen (Operand, [Definition])
genExpr (IR.FactorInt x  _) = return (cons $ C.Int 32 (fromIntegral x), [])
genExpr (IR.FactorReal x _) = return (cons $ C.Float (F.Double x), [])
genExpr (IR.FactorStr x _)  = undefined
genExpr (IR.FactorTrue _)   = return (cons $ C.Int 1 1, [])
genExpr (IR.FactorFalse _)  = return (cons $ C.Int 1 0, [])
genExpr (IR.Relation x1 op x2 _) = let 
    (t1,t2) = (IR.getType x1, IR.getType x2)
    cmpFloat y1 y2 = do
        fy1 <- if t1 == G.TYint then sitofp double y1 else return y1
        fy2 <- if t2 == G.TYint then sitofp double y2 else return y2
        case op of 
            OPless    -> fcmp FP.OLT fy1 fy2
            OPle      -> fcmp FP.OLE fy1 fy2
            OPgreater -> fcmp FP.OGT fy1 fy2
            OPge      -> fcmp FP.OGE fy1 fy2
            OPeq      -> fcmp FP.OEQ  fy1 fy2
            OPneq     -> fcmp FP.ONE  fy1 fy2
    cmpInt y1 y2 = do
        case op of 
            OPless    -> icmp IP.SLT y1 y2
            OPle      -> icmp IP.SLE y1 y2
            OPgreater -> icmp IP.SGT y1 y2
            OPge      -> icmp IP.SGE y1 y2
            OPeq      -> icmp IP.EQ  y1 y2
            OPneq     -> icmp IP.NE  y1 y2
    cmp = if t1 == G.TYreal || t2 == G.TYreal 
               then cmpFloat 
          else if t1 `elem` [G.TYint, G.TYbool] || t2 `elem` [G.TYint,G.TYbool]
               then cmpInt -- int and bool
               else undefined -- todo: other cases
    in do
        (y1, defs1) <- genExpr x1
        (y2, defs2) <- genExpr x2
        oper <- cmp y1 y2
        return (oper, defs1 ++ defs2)

genExpr (IR.Add x1 op x2 t) = do 
    (y1, defs1) <- genExpr x1
    (y2, defs2) <- genExpr x2
    oper <- case t of 
        G.TYbool -> undefined -- todo
        G.TYint  -> (if op == OPplus then iadd else isub) y1 y2
        G.TYreal -> do
            fy1 <- if IR.getType x1 == G.TYint then sitofp double y1 else return y1
            fy2 <- if IR.getType x2 == G.TYint then sitofp double y2 else return y2
            (if op == OPplus then fadd else fsub) fy1 fy2
    return (oper, defs1 ++ defs2)

genExpr (IR.Mult x1 op x2 t) = do
    (y1, defs1) <- genExpr x1
    (y2, defs2) <- genExpr x2
    oper <- case t of 
        G.TYbool -> undefined
        G.TYint  -> imul y1 y2
        G.TYreal -> do
            fy1 <- if IR.getType x1 == G.TYint then sitofp double y1 else return y1
            fy2 <- if IR.getType x2 == G.TYint then sitofp double y2 else return y2
            fmul fy1 fy2 -- todo replace fmul with case on op
    return (oper, defs1 ++ defs2)

genExpr (IR.Unary op x t) =  do
    (y, defs) <- genExpr x
    oper <- case op of 
        OPor    -> undefined
        OPplus  -> return y
        OPminus -> fst <$> (genExpr $ IR.Add (IR.FactorInt 0 G.TYbool) op x t)
    return (oper, defs)

genExpr (IR.FuncCall f xs t) = do
    (args, defs) <- mapM genExpr xs >>= (return.unzip)
    oper <- call (externf (toLLVMType t) (name' f)) args
    return (oper, concat defs)

genExpr (IR.FactorDesig (IR.Designator x _ xt) _) =
    (getvar (toShortBS x) (toLLVMType xt)) >>= load 
    >>= \oper -> return (oper, [])