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
    deriving (Show, Eq)

lookupVar   :: Env -> Ident -> Either TyErr Type
lookupVar (_, contexts) x = case (find f contexts) of
                    Nothing  -> Left  $ NotInScope x
                    Just ctx -> case lookup x ctx of
                        Nothing  -> Left  $ NotInScope x
                        Just t   -> Right t
    where f ctx = case (lookup x ctx) of 
            Nothing -> False
            Just _  -> True

lookupFun   :: Env -> Ident -> Either TyErr (Type, [Type])
lookupFun (sigs, _) x = 
    case lookup x sigs of
        Nothing -> Left $ NotInScope x
        Just f  -> Right f

newBlock  :: Env -> Either TyErr Env
newBlock (sig, contexts) = Right (sig, [] : contexts)

emptyEnv  :: Env
emptyEnv = ([], [])

getSig :: FuncDecl -> (Ident, (Type, [Type]))
getSig (FuncDecl x args t _) = (x, (t, map (\(a,b,c) -> b) args))

-- isLeft :: (Either a b) -> Bool 
-- isLeft (Left _) = True
-- isLeft _        = False
-- pickLeft x y = if isLeft x then x else y
-- fromRight (Right x) = x

isNum = (`elem` [TYint, TYreal])


typechk :: Env -> Statement -> Either TyErr Env
typechk env (Assignment (Designator x _) expr) = 
    gettype env expr >>= \t -> lookupVar env x >>= \xtype -> 
        if xtype == t
        then Right env 
        else Left $ TypeMismatch xtype t 

typechk env (StatementIf expr s1 ms2) =
    gettype env expr >>= \t ->
        if t /= TYbool 
        then Left $ TypeMismatch TYbool t
        else let tchk1 = typechk env s1
                 mtchk2 = (typechk env) <$> ms2
            in case mtchk2 of
            Just tchk2 -> tchk1 >> tchk2 >> Right env  
            Nothing    -> tchk1 >> Right env

typechk env (StatementFor i x1 _ x2 s) =
    lookupVar env i >>= \t -> 
        if (t /= TYint) && (t /= TYchar)
        then Left $ TypeMismatchOrd t
        else gettype env x1 >>= \t1 ->
            if t1 /= t 
            then Left $ TypeMismatch t t1
            else gettype env x2 >>= \t2 ->
                if t2 /= t
                then Left $ TypeMismatch t t2 
                else typechk env s


typechk env StatementEmpty = Right env

typechk env (StatementSeq xs) =
    foldr (>>) (Right env) (map (typechk env) xs)

gettype :: Env -> Expr -> Either TyErr Type
gettype env FactorTrue          = Right TYbool
gettype env FactorFalse         = Right TYbool
gettype env (FactorInt _)       = Right TYint
gettype env (FactorReal _)      = Right TYreal
gettype env (FactorStr _)       = Right TYstr
gettype env (FactorNot x)       = undefined

gettype env (FuncCall x (ExprList args)) = lookupFun env x >>=
    \(t, formalTs) -> 
        if length formalTs /= length args
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
    Right st -> typechk env st
    Left err -> error $ "Parse error:\n\t" ++ s ++ "\nin:" ++ show err