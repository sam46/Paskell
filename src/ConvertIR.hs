module ConvertIR where

import Grammar as AST
import qualified Intermediate as IR
import Paskell (parseProgram, parseDecl)
import TypeCheck (typechkProgram, TyErr, typechkDecl)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Utils (p')
import Data.List

-- Environment is a Func/Proc signatures + stack of Contexts for types and vars
type Env = (Sig, [Context], [TContext]) 
-- Function sig is return type + formal args types
type Sig = [(Ident, (Type, [(Type, CallByRef)]))]
-- Variables Context
type Context = [(Ident, Type)]
-- Type Context
type TContext = [(Ident, Type)]

varInContext :: Context -> Ident -> Bool
varInContext ctx x = case (lookup x ctx) of 
    Nothing -> False
    Just _  -> True

typeInContext :: TContext -> Ident -> Bool
typeInContext tctx x = case (lookup x tctx) of 
    Nothing -> False
    Just _  -> True

lookupVar :: Env -> Ident -> Type
lookupVar (_, contexts, _) x = case (find (`varInContext` x) contexts) of
                    Just ctx -> case lookup x ctx of
                        Just t   -> t

lookupFun :: Env -> Ident -> (Type, [(Type, CallByRef)])
lookupFun (sigs, _, _) x = 
    case lookup x sigs of Just f  -> f

lookupType :: Env -> Type -> Type
lookupType e@(_, _, contexts) (TYident x) = case (find (`typeInContext` x) contexts) of
                    Nothing  -> error $ "lookupType" -- Left  $ NotInScope x
                    Just ctx -> case lookup x ctx of
                        Nothing  -> error $ "lookupType" -- Left  $ NotInScope x
                        Just t   -> lookupType e t
lookupType _ t = t

newBlock :: Env -> Env
newBlock (sig, ctx, tctx) = (sig, [] : ctx, [] : tctx)

emptyEnv :: Env
emptyEnv = ([], [], [])

getSig :: Decl -> (Ident, (Type, [(Type, CallByRef)]))
getSig (DeclFunc x args t _) = (x, (t, map (\(_,b,c) -> (b,c)) args))

-- updateVar :: Env -> Ident -> Type -> Either TyErr Env

addVar :: Env -> Ident -> Type -> Env
addVar (sig , (c:cs), tctx) x t = (sig, ((x,t):c) : cs, tctx)

addFunc :: Env -> (Ident, (Type, [(Type, CallByRef)])) -> Env
addFunc (sigs, ctx, tctx) (x, rest) = case lookup x sigs of
        Nothing  -> ((x, rest) : sigs, ctx, tctx)


addType :: Env -> Ident -> Type -> Env
addType (sig, ctx, (c:cs)) x t = (sig, ctx, ((x,t):c) : cs)

convProgram :: Program -> IR.Program
convProgram (Program x b) = 
    IR.Program x (fst $ convBlock (newBlock emptyEnv) b) Void

convBlock :: Env -> Block -> (IR.Block, Env)
convBlock env (Block decls s)= let
    (decls', env') = convDecls env decls 
    s' = convStatement env' s in
    (IR.Block decls' s' Void, env)

convDecls :: Env -> [Decl] -> ([IR.Decl], Env)
convDecls env [] = ([], env)
convDecls env (d:ds) = let
    (ird, env')   = convDecl env d
    (irds, env'') = convDecls env' ds in
    (ird:irds, env'')

-- | Convert Decl to IR.Decl
convDecl :: Env -> Decl -> (IR.Decl, Env)
convDecl env (DeclVar xs) = let
    xs' =  map (\(x,t) -> (x, lookupType env t)) xs
    -- ^ [x,y,z] => [(x,tx),(y,ty),(z,tz)]
    addVar' (x,t) e = addVar e x t
    env'  = foldr addVar' env xs' in
    (IR.DeclVar xs' Void, env')
convDecl env (DeclFunc x params t b) = let
    params' = (x,t,False) : params in -- added hidden variable for return value
    convDeclFunc env (DeclFunc x params' t b)
convDecl env (DeclProc x params b) = 
    convDeclFunc env (DeclFunc x params Void b)
convDecl env (DeclType xs) = let 
    addType' (a',b') c' = addType c' a' b'
    env'  = foldr addType' env xs in
    (IR.DeclType xs Void, env')

convDeclFunc :: Env -> Decl -> (IR.Decl, Env)
convDeclFunc env (DeclFunc x params t b) = let
    r = lookupType env t
    resParams = resolveParamsType env params
    addVar' (a',b',_) c' = addVar c' a' b'
    env'  = addFunc env (getSig $ DeclFunc x resParams r b)
    env'' = foldr addVar' (newBlock env') resParams in
    (IR.DeclFunc x resParams r (fst $ convBlock env'' b) Void, env')

resolveParamsType :: Env -> [(Ident,Type,CallByRef)] -> [(Ident,Type,CallByRef)]
resolveParamsType env params = map (\(x,t,b) -> (x,lookupType env t,b)) params 

-- | Convert Statement to IR.Statement
convStatement :: Env -> Statement -> IR.Statement
convStatement env (Assignment des expr) =  -- Assignment
    IR.Assignment (convDesignator env des) (convExpr env expr) Void

-- | StatementIf
convStatement env (StatementIf expr s1 ms2) =
    IR.StatementIf (convExpr env expr) (convStatement env s1) ((convStatement env) <$> ms2) Void

-- | StatementFor
convStatement env (StatementFor i x1 b x2 s) = -- todo: add i to s's env?
    IR.StatementFor i (convExpr env x1) b (convExpr env x2) (convStatement env s) Void

-- | StatementWhile
convStatement env (StatementWhile expr s) = 
    IR.StatementWhile (convExpr env expr) (convStatement env s) Void

-- | StatementEmpty
convStatement env StatementEmpty = IR.StatementEmpty

-- | StatementSeq
convStatement env (StatementSeq xs) = IR.StatementSeq (map (convStatement env) xs) Void

-- | StatementRead (todo)
convStatement env (StatementRead xs) = error $ "convStatement (StatementRead): Not implemented"

-- | StatementWrite
convStatement env (StatementWrite xs) = IR.StatementWrite (map (convExpr env) xs) Void

-- | StatementWriteLn: Append NEWLINE at the end of string
convStatement env (StatementWriteLn xs) = convStatement env (StatementWrite $ xs ++ [FactorStr "\n"])

-- ProcCall
convStatement env (ProcCall f args) = 
    IR.ProcCall f args''' Void
    where   (_, sig) = lookupFun env f
            args''   = map (convExpr env) args -- convert to type annotaed IR args
            args'''  = map liftType (zip args'' sig) -- lift PassByRef args types to pointers, and typecast int to real when necessary
            liftType (expr, (ty,pbr)) = 
                let expr' = if ty == TYreal then expr {IR.getType = TYreal} else expr
                in if not pbr then expr'
                else let IR.FactorDesig x factty = expr'
                     in  IR.FactorDesig x (TYptr factty) 

-- | Convert Designator to IR.Designator
convDesignator :: Env -> Designator -> IR.Designator
convDesignator env (Designator x _) = IR.Designator x [] (lookupVar env x) 

-- | Convert Expr to IR.Expr
convExpr :: Env -> Expr -> IR.Expr
convExpr env FactorTrue        = IR.FactorTrue    TYbool    -- True
convExpr env FactorFalse       = IR.FactorFalse   TYbool    -- False
convExpr env (FactorInt x)     = IR.FactorInt  x  TYint     -- Int
convExpr env (FactorReal x)    = IR.FactorReal x  TYreal    -- Double
convExpr env (FactorStr x)     = IR.FactorStr  x  TYstr     -- String
convExpr env (FactorChar x)    = IR.FactorStr [x] TYstr     -- String >> char todo
convExpr env (FactorNot x)     = error $ "convExpr: FactorNot"
-- FunCall
convExpr env (FuncCall f args) = 
    IR.FuncCall f args''' fty
    where   (fty, sig) = lookupFun env f    -- lookup function f in env
            initialVal = case fty of        -- initial dummy value
                TYint  -> FactorInt 0       -- FactorInt
                TYstr  -> FactorStr "\0"    -- FactorStr
                TYbool -> FactorFalse       -- FactorFalse
                TYreal -> FactorReal 0.0    -- FactorReal
                TYchar -> FactorChar '\00'  -- FactorChar
            args'    = initialVal : args    -- add dummy arg
            args''   = map (convExpr env) args' -- convert to type annotaed IR args
            args'''  = map liftType (zip args'' sig) -- lift PassByRef args types to pointers, and typecast int to real when necessary
            liftType (expr, (ty, pbr)) = 
                let expr' = if ty == TYreal then expr {IR.getType = TYreal} else expr
                in if not pbr then expr'
                else let IR.FactorDesig x factty = expr'
                     in  IR.FactorDesig x (TYptr factty) 

convExpr env (FactorDesig des) = let 
    (Designator x _) = des
    in IR.FactorDesig (convDesignator env des) (lookupVar env x)
-- +, -
convExpr env (Unary op x) = let 
    x' = convExpr env x
    in IR.Unary op x' (IR.getType x')
-- <, <=, >, >=, ==
convExpr env (Relation x1 op x2) = let 
    x1' = convExpr env x1
    x2' = convExpr env x2 
    in IR.Relation x1' op x2' TYbool
-- Add
convExpr env (Add x1 op x2) = let
    x1' = convExpr env x1
    x2' = convExpr env x2
    t1  = IR.getType x1' 
    t2  = IR.getType x2' 
    t   = if op `elem` [OPplus, OPminus] 
          then if t1 == TYreal then t1 else t2 
          else TYbool
    in IR.Add x1' op x2' t
-- Mult
convExpr env (Mult x1 op x2) = let    
    x1' = convExpr env x1
    x2' = convExpr env x2
    t1  = IR.getType x1' 
    t2  = IR.getType x2' 
    t   | op == OPstar =
            if t1 == TYreal then t1 else t2 
        | op == OPdiv = TYreal
        | op `elem` [OPidiv, OPmod] = TYint
        | otherwise = TYbool
    in IR.Mult x1' op x2' t

-- | Typecheck program
chkConvProgram :: Program -> Either TyErr IR.Program
chkConvProgram p = case typechkProgram p of
    Left err -> Left err
    Right () -> Right $ convProgram p

chkConvFile :: String -> IO ()
chkConvFile path = let p = parseFromFile parseProgram path
    in p >>= \pp -> print $ chkConvProgram <$> pp

chkConvProgram' :: String -> Either String IR.Program
chkConvProgram' s = let p = p' parseProgram s in
    case p of Left x -> Left $ show x
              Right pp -> case chkConvProgram pp of 
                            Left y -> Left $ show y
                            Right d -> Right d


chkConvDecl :: Decl -> Either TyErr IR.Decl
chkConvDecl d = case typechkDecl ([],[],[]) d of
    Left err -> Left err
    _        -> Right $ fst $ convDecl ([],[],[]) d

chkConvDecl' :: String -> Either String IR.Decl
chkConvDecl' s = let p = p' parseDecl s in
    case p of Left x -> Left $ show x
              Right pp -> case chkConvDecl pp of 
                            Left y -> Left $ show y
                            Right d -> Right d