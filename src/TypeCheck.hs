module TypeCheck where

import Grammar
import Paskell 
import Utils (p')
import Data.List
import Control.Monad (msum)

-- Environment is a Func/Proc signatures + stack of Contexts
type Env = (Sig, [Context]) 
-- Function sig is return type + formal args types
type Sig = [(Ident, (Type, [Type]))]
type Context = [(Ident, Type)]

data TyErr = NotInScope Ident
    | TypeMismatch Type Type 
    | TypeMismatchOrd Type
    | TypeMismatchNum Type
    | ArgCountMismatch Int
    | ArgTypeMismatch Type Type
    | CondTypeMismatch Type
    | VarRedecl Ident
    | FuncRedecl Ident
    deriving (Show, Eq)


varInContext :: Context -> Ident -> Bool
varInContext ctx x = case (lookup x ctx) of 
    Nothing -> False
    Just _  -> True

lookupVar   :: Env -> Ident -> Either TyErr Type
lookupVar (_, contexts) x = case (find (`varInContext` x) contexts) of
                    Nothing  -> Left  $ NotInScope x
                    Just ctx -> case lookup x ctx of
                        Nothing  -> Left  $ NotInScope x
                        Just t   -> Right t

lookupFun   :: Env -> Ident -> Either TyErr (Type, [Type])
lookupFun (sigs, _) x = 
    case lookup x sigs of
        Nothing -> Left $ NotInScope x
        Just f  -> Right f

newBlock  :: Env -> Env
newBlock (sig, contexts) = (sig, [] : contexts)

emptyEnv  :: Env
emptyEnv = ([], [])

getSig :: Decl -> (Ident, (Type, [Type]))
getSig (DeclFunc x args t _) = (x, (t, map (\(_,b,_) -> b) args))

-- updateVar :: Env -> Ident -> Type -> Either TyErr Env

addVar :: Env -> Ident -> Type -> Either TyErr Env
addVar (sig , (c:cs)) x t = if varInContext c x
    then Left $ VarRedecl x
    else Right $ (sig, ((x,t):c) : cs)

addFunc :: Env -> (Ident, (Type, [Type])) -> Either TyErr Env
addFunc (sigs, ctx) (x, rest) = 
    case lookup x sigs of
        Just _   -> Left $ FuncRedecl x
        Nothing  -> Right ((x, rest) : sigs, ctx)

-- isLeft :: (Either a b) -> Bool 
-- isLeft (Left _) = True
-- isLeft _        = False
-- pickLeft x y = if isLeft x then x else y
-- fromRight (Right x) = x

isNum = (`elem` [TYint, TYreal])

typechkProgram :: Program -> Either TyErr ()
typechkProgram (Program _ b) = typechkBlock (newBlock emptyEnv) b >> return () 

typechkBlock :: Env -> Block -> Either TyErr Env
typechkBlock env (Block ds s) = 
    typechkDecls env ds >>= \e -> typechkStatement e s

typechkDecls :: Env -> [Decl] -> Either TyErr Env
typechkDecls env [] = Right env
typechkDecls env (d:ds) = 
    typechkDecl env d >>= \e -> typechkDecls e ds

typechkDecl :: Env -> Decl -> Either TyErr Env
typechkDecl env (DeclVar []) = Right env
typechkDecl env (DeclVar (d:ds)) = let (x,t) = d in
    (addVar env x t) >>= \e -> typechkDecl e (DeclVar ds) 
typechkDecl env (DeclFunc x params t b) = typechkDeclFunc env (DeclFunc x params' t b)
    where params' = (x,t,False) : params -- added hidden variable for return value
typechkDecl env (DeclProc x params b) = typechkDeclFunc env (DeclFunc x params Void b)

typechkTypeDecl :: Env -> TypeDecl -> Either TyErr Env
typechkTypeDecl env _ = undefined

typechkConstDecl :: Env -> ConstDecl -> Either TyErr Env
typechkConstDecl env _ = undefined

typechkDeclFunc :: Env -> Decl -> Either TyErr Env
typechkDeclFunc env (DeclFunc x params t b) =
    let sig = getSig (DeclFunc x params t b) in
    (addFunc env sig) >>= \e ->
        typechkDecls (newBlock e) (map (\(i,ty,_) -> DeclVar [(i, ty)]) params) >>= \e2 ->
            typechkBlock e2 b >> Right e


typechkStatement :: Env -> Statement -> Either TyErr Env
typechkStatement env (Assignment (Designator x _) expr) = 
    gettype env expr >>= \t -> lookupVar env x >>= \xtype -> 
        if xtype == t
        then Right env 
        else Left $ TypeMismatch xtype t 

typechkStatement env (StatementIf expr s1 ms2) =
    gettype env expr >>= \t ->
        if t /= TYbool 
        then Left $ TypeMismatch TYbool t
        else let tchk1 = typechkStatement env s1
                 mtchk2 = (typechkStatement env) <$> ms2
            in case mtchk2 of
            Just tchk2 -> tchk1 >> tchk2 >> Right env  
            Nothing    -> tchk1 >> Right env

typechkStatement env (StatementFor i x1 _ x2 s) = -- todo: add i to s's env?
    lookupVar env i >>= \t -> 
        if (t /= TYint) && (t /= TYchar)
        then Left $ TypeMismatchOrd t
        else gettype env x1 >>= \t1 ->
            if t1 /= t 
            then Left $ TypeMismatch t t1
            else gettype env x2 >>= \t2 ->
                if t2 /= t
                then Left $ TypeMismatch t t2 
                else typechkStatement env s

typechkStatement env (StatementWhile expr s) = 
    gettype env expr >>= \t -> 
    if t /= TYbool then Left $ CondTypeMismatch t
    else typechkStatement env s

typechkStatement env StatementEmpty = Right env

typechkStatement env (StatementSeq xs) =
    foldr (>>) (Right env) (map (typechkStatement env) xs)

typechkStatement env (StatementWrite xs) = 
    foldr (>>) (Right env) (map (gettype env) xs)
typechkStatement env (StatementWriteLn xs) = 
    typechkStatement env (StatementWrite xs)

typechkStatement env _ = Right env -- todo


gettype :: Env -> Expr -> Either TyErr Type
gettype env FactorTrue          = Right TYbool
gettype env FactorFalse         = Right TYbool
gettype env (FactorInt _)       = Right TYint
gettype env (FactorReal _)      = Right TYreal
gettype env (FactorStr _)       = Right TYstr
gettype env (FactorNot x)       = undefined

gettype env (FuncCall x args) = lookupFun env x >>=
    \(t, formalTs) -> 
        if length formalTs /= length args + 1
        then Left $ ArgCountMismatch (length formalTs)
        else foldr (>>) (Right t) (map f (zip args formalTs)) 
    where f (ex, formalT) = (gettype env ex) >>= \exT -> 
            if exT /= formalT
            then Left $ ArgTypeMismatch formalT exT
            else Right exT

gettype env (FactorDesig (Designator x _)) = 
    lookupVar env x

gettype env (Unary op x) = 
    gettype env x >>= \t ->
        if isNum t then Right t 
        else Left $ TypeMismatchNum t

gettype env (Relation x1 op x2) =
    t1 >>= \v1 -> t2 >>= \v2 ->
        if (v1 == v2) || (isNum v1 && isNum v2)
        then Right TYbool
        else Left $ TypeMismatch v1 v2
    where [t1, t2] = (gettype env) <$> [x1, x2]

gettype env (Add x1 op x2)
    | op `elem` [OPplus, OPminus] =
        t1 >>= \v1 -> t2 >>= \v2 ->
            if not (isNum v1 && isNum v2)
            then Left $ TypeMismatchNum (if isNum v1 then v1 else v2)
            else if v1 == TYreal then t1 else t2
    | otherwise = 
        t1 >>= \v1 -> t2 >>= \v2 ->
            if (v1 /= TYbool) || (v2 /= TYbool)
            then Left $ TypeMismatch TYbool (if v1 /= TYbool then v1 else v2)
            else t1
    where [t1, t2] = (gettype env) <$> [x1, x2]

gettype env (Mult x1 op x2)
    | op `elem` [OPstar, OPdiv] =
        t1 >>= \v1 -> t2 >>= \v2 ->
            if not (isNum v1 && isNum v2)
            then Left $ TypeMismatchNum (if isNum v1 then v1 else v2)
            else if v1 == TYreal then t1 else t2
    | otherwise = 
        t1 >>= \v1 -> t2 >>= \v2 ->
            if (v1 /= TYbool) || (v2 /= TYbool)
            then Left $ TypeMismatch TYbool (if v1 /= TYbool then v1 else v2)
            else t1
    where [t1, t2] = (gettype env) <$> [x1, x2]

typechkStr s env = case p' parseStatement s of 
    Right st -> typechkStatement env st
    Left err -> error $ "Parse error:\n\t" ++ s ++ "\nin:" ++ show err
