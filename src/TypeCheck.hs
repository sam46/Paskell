module TypeCheck where

import Grammar
import Paskell 
import Utils (p')
import Data.List

type Env = [(Ident, Type)]

-- isLeft :: (Either a b) -> Bool 
-- isLeft (Left _) = True
-- isLeft _        = False
-- pickLeft x y = if isLeft x then x else y
-- fromRight (Right x) = x

isNum = (`elem` [TYint, TYreal])

-- Convert a variable lookup from Maybe to Either
lookupIdent :: Ident -> Env -> Either String Type
lookupIdent x env = case (lookup x env) of 
    Nothing -> Left $ "Unknown variable " ++ show x
    Just t  -> Right t

typechk :: Env -> Statement -> Either String Env
typechk env (Assignment (Designator x _) expr) = 
    gettype env expr >>= \t -> lookupIdent x env >>= \xtype -> 
        if xtype == t
        then Right env 
        else Left $ "Can't assign " ++ 
            (show t) ++ " to " ++ show xtype

typechk env (StatementIf expr s1 ms2) =
    gettype env expr >>= \t ->
        if t /= TYbool 
        then Left $ "Expecting boolean in IF condition, got " ++ show t 
        else let tchk1 = typechk env s1
                 mtchk2 = (typechk env) <$> ms2
            in case mtchk2 of
            Just tchk2 -> tchk1 >> tchk2 >> Right env  
            Nothing    -> tchk1 >> Right env

typechk env (StatementFor i x1 _ x2 s) =
    lookupIdent i env >>= \t -> 
        if (t /= TYint) && (t /= TYchar)
        then Left $ "Ordinal type expected, got " ++ show t
        else gettype env x1 >>= \t1 ->
            if t1 /= t 
            then Left $ "Can't assign " ++ (show t1) ++ " to " ++ show t
            else gettype env x2 >>= \t2 ->
                if t2 /= t
                then Left $ (show t) ++ " expected, got " ++ show t2 
                else typechk env s


typechk env StatementEmpty = Right env

typechk env (StatementSeq xs) =
    foldr (>>) (Right env) (map (typechk env) xs)

gettype :: Env -> Expr -> Either String Type
gettype env FactorTrue          = Right TYbool
gettype env FactorFalse         = Right TYbool
gettype env (FactorInt _)       = Right TYint
gettype env (FactorReal _)      = Right TYreal
gettype env (FactorStr _)       = Right TYstr
gettype env (FactorNot x)       = undefined
gettype env (FactorFuncCall fc) = undefined

gettype env (FactorDesig (Designator x _)) = 
    lookupIdent x env

gettype env (Unary op x) = 
    gettype env x >>= \t ->
        if isNum t then Right t 
        else Left $ (show op) ++ " operand must be integer or real"

gettype env (Relation x1 op x2) =
    t1 >>= \v1 -> t2 >>= \v2 ->
        if (v1 == v2) || (isNum v1 && isNum v2)
        then Right TYbool
        else Left $ (show op) ++ " operands must be boolean"
    where [t1, t2] = (gettype env) <$> [x1, x2]

gettype env (Add x1 op x2)
    | op `elem` [OPplus, OPminus] =
        t1 >>= \v1 -> t2 >>= \v2 ->
            if not (isNum v1 && isNum v2)
            then Left $ (show op) ++ " operands must be integer or real"
            else if v1 == TYreal then t1 else t2
    | otherwise = 
        t1 >>= \v1 -> t2 >>= \v2 ->
            if (v1 /= TYbool) || (v2 /= TYbool)
            then Left $ (show op) ++ " operands must be boolean"
            else t1
    where [t1, t2] = (gettype env) <$> [x1, x2]

gettype env (Mult x1 op x2)
    | op `elem` [OPstar, OPdiv] =
        t1 >>= \v1 -> t2 >>= \v2 ->
            if not (isNum v1 && isNum v2)
            then Left $ (show op) ++ " operands must be integer or real"
            else if v1 == TYreal then t1 else t2
    | otherwise = 
        t1 >>= \v1 -> t2 >>= \v2 ->
            if (v1 /= TYbool) || (v2 /= TYbool)
            then Left $ (show op) ++ " operands must be boolean"
            else t1
    where [t1, t2] = (gettype env) <$> [x1, x2]

typechkStr s env = case p' parseStatement s of 
    Right st -> typechk env st
    Left err -> error $ "Parse error:\n\t" ++ s ++ "\nin:" ++ show err