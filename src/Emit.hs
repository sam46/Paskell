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
import LLVM.AST.Type (ptr, i8)
import LLVM.AST.AddrSpace
import LLVM.AST.Attribute (ParameterAttribute)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.Analysis
import LLVM.Transforms
import LLVM.Internal.PassManager
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module
import LLVM.AST.ParameterAttribute
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short hiding (length)
import qualified ConvertIR as Conv
import qualified Intermediate as IR
import Codegen

import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
-- Type conversions
-------------------------------------------------------------------------------

toShortBS =  toShort . BS.pack 
toString = BS.unpack . fromShort
name' = Name . toShortBS

toLLVMType :: G.Type -> Type
toLLVMType t = case t of 
    G.TYint   -> int
    G.TYbool  -> bool
    G.TYreal  -> double
    G.Void    -> void'
    G.TYstr   -> str
    G.TYchar  -> char
    G.TYptr t -> ptr $ toLLVMType t
    G.TYarr sz ty
        -> case ty of
            G.TYint   -> arrayType sz int
            G.TYreal  -> arrayType sz double
            G.TYchar  -> arrayType sz char
            G.TYarr _ _ -> error $ "toLLVMType: 2D Array"
    _         -> error $ "TYident wasn't resolved.\n" ++ (show t)

-- add ParameterAtribute to an argument given the argument and 
-- it's corrosponding Operand
addParamAttr :: IR.Expr -> Operand -> (Operand, [ParameterAttribute])
addParamAttr expr oper 
    = let ty = IR.getType expr in 
        case ty of 
            G.TYptr t -> (oper, [Dereferenceable (typeSize t)])
            _         -> (oper, []) 

typeSize ty = case ty of
    G.TYreal -> 8
    G.TYint  -> 4
    G.TYchar -> 1
    _        -> 1

isPtrPtr :: Operand -> Bool  -- checks for a double pointer
isPtrPtr oper = case oper of 
    LocalReference (PointerType (PointerType _ _) _) _ -> True
    _ -> False

-------------------------------------------------------------------------------
-- Top-level interface 
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- LLVM-IR and LLVM-AST given IR
codegen :: AST.Module -> IR.Program -> IO (AST.Module, String)
codegen mod pr = withContext $ \context ->
    liftIO $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        return (newast, BS.unpack llstr)
    where 
        newast = runLLVM mod (genProgram pr)

-- | Same as codegen, applying optimization passes to generated LLVM module
codegen' :: AST.Module -> IR.Program -> IO (AST.Module, String)
codegen' mod pr = withContext $ \context ->
    liftIO $ withModuleFromAST context newast $ \m -> do
        withPassManager passes $ \pm -> do
            runPassManager pm m
        llstr <- moduleLLVMAssembly m
        return (newast, BS.unpack llstr)
    where 
        newast = runLLVM mod (genProgram pr)

-- | List of LLVM optimization passes
passes :: PassSetSpec
passes = defaultPassSetSpec { 
    transforms = [
        -- c
          ConstantPropagation
        , CorrelatedValuePropagation
        -- d
        , DeadCodeElimination
        , DeadInstructionElimination
        , DeadStoreElimination
        -- g
        , GlobalDeadCodeElimination
        -- i
        , InstructionCombining
        -- l
        , LoopDeletion
        -- m
        , MemcpyOptimization
        -- p
        , PromoteMemoryToRegister
        -- s
        , SimplifyControlFlowGraph
        , SparseConditionalConstantPropagation
        , StripSymbols True
        -- t
        , TailCallElimination
      ]}

-- LLVM-IR given parse tree
printllvm :: G.Program -> ShortByteString -> IO String
printllvm ast path = let ir = Conv.convProgram ast in
    do (llvmast, llstr) <- codegen' (emptyModulePath "MainModule" path) ir
       return llstr

-------------------------------------------------------------------------------
-- Declarations and Blocks
-------------------------------------------------------------------------------

genDeclFunc :: IR.Decl -> LLVM ()
genDeclFunc (IR.DeclFunc x args retty blk _) = do
    define (toLLVMType retty) (toShortBS x) (toSig args) body
    where 
        toSig = map (\(a,b,c) -> 
                        (toLLVMType (if c 
                                    then G.TYptr b 
                                    else b), 
                        name' a, 
                        if c 
                        then [Dereferenceable (typeSize b)] 
                        else []))
        body = do
            entry' <- addBlock "entry"
            setBlock entry'
            forM args $ \(i,t,byref) -> do
                let ty = toLLVMType $ if byref then G.TYptr t else t
                var <- alloca ty
                store var (local (ty) (name' i))
                assign (toShortBS i) var
            defs <- genBlock blk
            -- return dummy variable value for functions, or void for procedures 
            if retty == G.Void 
                then retvoid
                else getvar (toShortBS x) (toLLVMType retty) 
                     >>= load (toLLVMType retty) >>= ret
            return defs

-- used for local var decls only 
genDeclVar :: IR.Decl -> Codegen ()
genDeclVar (IR.DeclVar xs _) = do
    forM xs $ \(i,t) -> do
        var <- alloca' (toLLVMType $ t)     {- ptr -}
        assign (toShortBS i) var
    return ()
genDeclVar _ = return ()

genDeclGlob :: IR.Decl -> LLVM ()
genDeclGlob d@(IR.DeclVar _ _) = genDeclVarGlob d
genDeclGlob d@(IR.DeclFunc _ _ _ _ _) = genDeclFunc d 
genDeclGlob _ = return () 

genDeclVarGlob :: IR.Decl -> LLVM ()
genDeclVarGlob (IR.DeclVar xs _) = do
    forM xs $ \(i,t) -> gvar (toLLVMType t) (name' i)
    return ()
genDeclVarGlob _ = return ()

-- generate entry point main()
genMain :: IR.Statement -> LLVM ()
genMain s = genDeclFunc (IR.DeclFunc "main" args (G.TYint) (IR.Block [] s G.Void) G.Void) 
    where args = [("main", G.TYint, False)] -- dummy return value variable

genProgram :: IR.Program -> LLVM ()
genProgram (IR.Program p (IR.Block ds s _) _) = do
    addDefns [printf, malloc, scanf, free]
    forM ds genDeclGlob
    genMain s

genBlock :: IR.Block -> Codegen [Definition]
genBlock (IR.Block ds s _) = do
    forM ds genDeclVar
    genStatement s

-------------------------------------------------------------------------------
-- Statements
-------------------------------------------------------------------------------

genStatement :: IR.Statement -> Codegen [Definition]
genStatement (IR.StatementEmpty) = return []
genStatement (IR.StatementSeq xs _) = (forM xs genStatement) >>= (return . concat)
genStatement (IR.Assignment (IR.Designator x IR.DesigPropNone xt) expr _) = do
    (rhs, defs) <- genExpr expr
    let ty = toLLVMType xt
    var <- getvar (toShortBS x) (ptr $ ty)  -- var is a pointer {- ptr -}
    if not (isPtrPtr var)    
        then store var rhs -- store value at memory referred to by pointer
        else do            -- if var is a pointer to pointer, this means we have something like *x = 123 and we should derference the pointer first
                ptrv <- load (ptr $ void') var {- ptr -}
                store ptrv rhs
    return defs

-- | Assignment Array (todo: fix bug with pointer)
genStatement (IR.Assignment (IR.Designator x (IR.DesigPropArray xexpr) xt) expr _) = do
    (rhs, defs) <- genExpr expr
    (offset, odefs) <- genExpr (head xexpr)
    let ty = toLLVMType xt
    xop <- getvar (toShortBS x) (ptr $ ty)
    getElementPtr' (ty) (xop) [offset] -- ? --
    return $ defs ++ odefs

-- | Generate IF statements
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

-- | Generate For Loops
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
    where loopvar = IR.Designator x IR.DesigPropNone (IR.getType expr1)
          varfactor = IR.FactorDesig loopvar (IR.getType expr1)
          (optest, opstep) = if todownto then (OPle, OPplus) else (OPge, OPminus)
          step = IR.Add varfactor opstep (IR.FactorInt 1 G.TYint) (IR.getType expr1)

-- | Generate While statements
genStatement (IR.StatementWhile expr s _) = do
    wtest <- addBlock "while.test"
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


-- | Generate Read Statements
genStatement (IR.StatementRead d@(IR.Designator des desprop desty) ty) = do
    let expr' = (IR.FactorStr (formatstr' ty) G.TYstr) : [IR.FactorDesig d (G.TYptr ty)]
    (args, defs) <- mapM genExpr expr' >>= (return . unzip)
    callNoCast (externf scanfTy (name' "scanf")) (zipWith addParamAttr expr' args)
    return $ concat defs
    
-- | Generate Procedure calls
genStatement (IR.ProcCall f xs t) = do
    (args, defs) <- mapM genExpr xs >>= (return . unzip)
    call' (externf fnty (name' f)) (zipWith addParamAttr xs args)
    return $ concat defs
    where fnty = toLLVMfnType (toLLVMType t) (map (toLLVMType . IR.getType) xs)

-- | Generate Write statements
genStatement (IR.StatementWrite xs' _) = do
    (args, defs) <- mapM genExpr xs >>= (return . unzip)
    callNoCast (externf printfTy (name' "printf")) (zipWith addParamAttr xs args)
    return $ concat defs
    where fstr = (foldr (++) "" (map (formatstr . IR.getType) xs')) ++ "\00"
          xs = (IR.FactorStr fstr G.TYstr) : xs' -- add printf format string to arguments

-- | Generate New statements
genStatement (IR.StatementNew ident expr ty) = do
    (args, defs) <- genExpr expr
    callNoCast (externf mallocTy (name' "malloc")) ([])
    -- error $ "genStatement (IR.StatementNew): Not implemented"
    return $ defs

-- | Generate Dispose statements
genStatement (IR.StatementDispose ident isArrayType ty) = do
    let expr = IR.FactorDesig (IR.Designator ident (IR.DesigPropArray []) ty) (G.TYptr ty)
    (args, defs) <- genExpr (expr)
    args' <- bitcast args (ptr $ i8)
    callNoCast' (externf freeTy (name' "free")) [(args', [])]
    return $ defs
    -- error $ "genStatement (IR.StatementDispose): Not implemented"

-- | Not recognized Statement
genStatement _ = error $ "genStatement: Undefined"

-- | printf format specifiers
formatstr :: G.Type -> String
formatstr (G.TYint)   = "%d"
formatstr (G.TYstr)   = "%s"
formatstr (G.TYreal)  = "%lf"
formatstr (G.TYbool)  = "%d"
formatstr (G.TYchar)  = "%c"
formatstr (G.TYptr ty) = "Pointer: " ++ formatstr ty    -- todo: fix this (dereference)
formatstr (G.TYarr _ ty) = "Array: " ++ formatstr ty    -- todo: fix this (dereference)

-- | scanf format specifiers
formatstr' :: G.Type -> String
formatstr' G.TYchar = " %c"     -- discard newline
formatstr' t = formatstr t

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

-- returns %x for final expression value, and stores any intermediate instructions in the block
genExpr :: IR.Expr -> Codegen (Operand, [Definition])
genExpr (IR.FactorInt x  _) = return (cons $ C.Int 32 (fromIntegral x), [])
genExpr (IR.FactorReal x _) = return (cons $ C.Float (F.Double x), [])
genExpr (IR.FactorStr x _)  = do 
    strglobal <- freshStrName
    def <- return $ gstrVal' (name' strglobal) x'
    (ConstantOperand ptrv) <- getvar (toShortBS strglobal) (ptr $ arrayType (length x') char)
    oper <- return $ cons $ C.GetElementPtr True (ptrv) [C.Int 32 0, C.Int 32 0]
    return (oper, [def])
    where x' = if last x /= '\00' then x ++ "\00" else x

genExpr (IR.FactorTrue _)   = return (cons $ C.Int 1 1, []) -- True
genExpr (IR.FactorFalse _)  = return (cons $ C.Int 1 0, []) -- False
-- | Generate Relational operations
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
            OPeq      -> fcmp FP.OEQ fy1 fy2
            OPneq     -> fcmp FP.ONE fy1 fy2
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
               else error $ "genExpr: IR.Relation" -- todo: other cases
    in do
        (y1, defs1) <- genExpr x1
        (y2, defs2) <- genExpr x2
        oper <- cmp y1 y2
        return (oper, defs1 ++ defs2)

genExpr (IR.Add x1 op x2 t) = do 
    (y1, defs1) <- genExpr x1
    (y2, defs2) <- genExpr x2
    oper <- case t of 
        G.TYbool -> bor y1 y2
        G.TYint  -> (if op == OPplus then iadd else isub) y1 y2
        G.TYreal -> do
            fy1 <- if IR.getType x1 == G.TYint then sitofp double y1 else return y1
            fy2 <- if IR.getType x2 == G.TYint then sitofp double y2 else return y2
            (if op == OPplus then fadd else fsub) fy1 fy2
    return (oper, defs1 ++ defs2)

-- | Generate Multiplicative operations
genExpr (IR.Mult x1 op x2 t) = do
    let (t1,t2) = (IR.getType x1, IR.getType x2)
    (y1, defs1) <- genExpr x1
    (y2, defs2) <- genExpr x2
    oper <- case op of 
        OPdiv -> do -- /
            fy1 <- sitofp double y1
            fy2 <- sitofp double y2
            fdiv fy1 fy2 
        OPstar -> do -- *
            fy1 <- if t1 == G.TYint then sitofp double y1 else return y1
            fy2 <- if t2 == G.TYint then sitofp double y2 else return y2
            if t1 == G.TYreal || t2 == G.TYreal 
                then fmul fy1 fy2
                else imul y1 y2
        OPidiv -> idiv y1 y2 -- div
        OPmod  -> imod y1 y2 -- mod
        OPand  -> band y1 y2 -- and
    return (oper, defs1 ++ defs2)

genExpr (IR.Unary op x t) =  do
    (y, defs) <- genExpr x
    oper <- case op of 
        OPor    -> error $ "genExpr: IR.Unary"
        OPplus  -> return y -- +x = x
        OPminus -> fst <$> (genExpr $ IR.Add (IR.FactorInt 0 G.TYint) op x t) -- -x = 0 - x
    return (oper, defs)

-- | Generate Function Call
genExpr (IR.FuncCall f xs t) = do
    (args, defs) <- mapM genExpr xs >>= (return.unzip)
    oper <- call (externf fnty (name' f)) (zipWith addParamAttr xs args)
    return (oper, concat defs)
    where fnty = toLLVMfnType (toLLVMType t) (map (toLLVMType . IR.getType) xs)
        -- error $ (show $ map IR.getType xs)
          
genExpr (IR.FactorDesig (IR.Designator x IR.DesigPropNone xt) dt) =
    (getvar (toShortBS x) (ptr $ toLLVMType xt)) {-- ptr --}
    >>= (if dt == xt then load (toLLVMType dt) else return . id)
    >>= \oper -> return (oper, [])

genExpr (IR.FactorDesig (IR.Designator x (IR.DesigPropArray (expr : exprs)) xt) dt) = do
    (offset, _) <- genExpr expr
    let ty = toLLVMType dt
    xop <- getvar (toShortBS x) (ptr $ ty)
    oper <- if dt == xt then getElementPtr' (ty) xop [offset] else return offset
    return (oper, [])

genExpr (IR.FactorDesig (IR.Designator x _ xt) dt) =
    (getvar (toShortBS x) (ptr $ toLLVMType xt)) {-- ptr --}
    >>= (if dt == xt then load (toLLVMType xt) else return . id)
    >>= \oper -> return (oper, [])