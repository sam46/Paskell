{-# LANGUAGE OverloadedStrings #-}

module Emit where 

import qualified Intermediate as IR
import qualified Grammar as G ( Type(..) )
import Grammar (OP, Ident, IdentList,
    VarDecl, TypeDecl, CallByRef, ToDownTo)

import Control.Monad.Except
import Control.Applicative

import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module
import LLVM.AST.ParameterAttribute
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short
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

toParamList params = map mapParam params
    where mapParam (x,t,byref) = 
            Parameter (toLLVMType t) (name' x) (if byref then [Dereferenceable 4] else [])
-------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [IR.Decl] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftIO $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn (BS.unpack llstr)
    return newast
  where
    modn    = mapM genDeclFunc fns
    newast = runLLVM mod modn
-------------------------

genDecl :: IR.Decl -> Codegen ()
genDecl d@(IR.DeclVar xs _) = genDeclVar d >> return ()
-- genDecl d@(IR.DeclFunc x args retty blk _) = genDeclFunc d

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
            genBlock blk
            ret (cons (C.Int 32 (fromIntegral 1)))


genDeclVar (IR.DeclVar xs _) = forM xs $ \(i,t) -> do
    var <- alloca (toLLVMType t)
    assign (toShortBS i) var

genBlock :: IR.Block -> Codegen ()
genBlock (IR.Block ds s _) = do
    forM ds genDecl
    genStatement s


genStatement :: IR.Statement -> Codegen ()
genStatement (IR.StatementEmpty) = return ()
genStatement (IR.StatementSeq xs _) = (forM xs genStatement) >> return ()
genStatement (IR.Assignment (IR.Designator x _ xt) expr _) = do
    -- get %r value for rhs
    -- store  %r, pointer to x
    rhs <- genExpr expr
    var <- getvar $ toShortBS x
    store var rhs

-- returns %x for final expression value, and stores any intermediate instructions in the block
genExpr :: IR.Expr -> Codegen Operand
genExpr (IR.FactorInt x  _) = return $ cons $ C.Int 32 (fromIntegral x)
genExpr (IR.FactorReal x _) = return $ cons $ C.Float (F.Double x)
genExpr (IR.FactorStr x _)  = undefined
genExpr (IR.FactorTrue _)   = return $ cons $ C.Int 1 1
genExpr (IR.FactorFalse _)  = return $ cons $ C.Int 1 0
genExpr (IR.Relation x1 op x2 _) = undefined
genExpr (IR.Add x1 op x2 t) = do 
    y1 <- genExpr x1
    y2 <- genExpr x2
    case t of 
        G.TYbool -> undefined
        G.TYint  -> iadd y1 y2
        G.TYreal -> 
            let (mn,mx) = if x1 < x2 then (y1,y2) else (y2,y1) -- mn is TYint
            in do fmn <- sitofp double mn
                  fadd fmn mx
            

genExpr (IR.Mult x1 op x2 t) = do
    y1 <- genExpr x1
    y2 <- genExpr x2
    case t of 
            G.TYbool -> undefined
            G.TYint  -> imul y1 y2
            G.TYreal -> 
                let (mn,mx) = if x1 < x2 then (y1,y2) else (y2,y1) -- mn is TYint
                in do fmn <- sitofp double mn
                      fmul fmn mx -- todo replace fmul with case on op

genExpr (IR.Unary op x _) = undefined
