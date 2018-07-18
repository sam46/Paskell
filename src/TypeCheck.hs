module TypeCheck where

import Grammar
import Paskell 
import Utils (p')
import Data.List

type Env = [(Ident, Type)]

typechk :: Env -> Statement -> Env
typechk env (Assignment (Designator x _) expr) = 
    case (lookup x env) >>= return.(== (gettype env expr)) of 
        Just True -> env
        _         -> error "type error" 

typechk env (StatementIf x s1 ms2) =
    if (gettype env x) /= TYbool 
    then error "If condition must be bool" 
    else let tchk1 = typechk env s1
             tchk2 = case ms2 of Just s2 -> Just $ typechk env s2
                                 Nothing -> Nothing
         in seq tchk2 env

gettype :: Env -> Expr -> Type
gettype env (FactorInt _ ) = TYint
gettype env (FactorReal _) = TYreal
gettype env (FactorStr _) = TYstr
gettype env FactorTrue = TYbool
gettype env FactorFalse = TYbool
gettype env (FactorNot x) = undefined
gettype env (FactorFuncCall fc) = undefined
gettype env (FactorDesig (Designator x _)) = 
    case lookup x env of 
        Nothing -> error $ "unkonw identifier " ++ (show x) 
        Just t  -> t
gettype env (Unary op x) = 
    if t `notElem` [TYint, TYreal]
    then error "expression type error"
    else t
    where t = gettype env x
gettype env (Add x1 op x2)
    | op `elem` [OPplus, OPminus] =
        if (t1 `notElem` [TYint, TYreal]) || (t2 `notElem` [TYint, TYreal])
        then error "expression type error"
        else if t1 == TYreal then t1 else t2
    | otherwise = 
        if (t1 /= TYbool) || (t2 /= TYbool)
        then error "expression type error"
        else TYbool
    where t1 = gettype env x1
          t2 = gettype env x2
gettype env (Mult x1 op x2)
    | op `elem` [OPstar, OPdiv] =
        if (t1 `notElem` [TYint, TYreal]) || (t2 `notElem` [TYint, TYreal])
        then error "expression type error"
        else if t1 == TYreal then t1 else t2
    | otherwise = 
        if (t1 /= TYbool) || (t2 /= TYbool)
        then error "expression type error"
        else TYbool
    where t1 = gettype env x1
          t2 = gettype env x2
gettype env (Relation x1 op x2) = -- todo more intricate type comparsions
    if (t1 == t2) ||
       ((t1 `elem` [TYint, TYreal]) && (t2 `elem` [TYint, TYreal])) ||
       ((t1 `elem` [TYchar, TYstr]) && (t2 `elem` [TYchar, TYstr]))
    then TYbool
    else error "type mismatch"
    where t1 = gettype env x1
          t2 = gettype env x2
