module TypeCheck where

import Grammar
import Paskell 
import Utils (p')
import Data.List

type Env = [(Ident, Type)]

isLeft :: (Either a b) -> Bool 
isLeft (Left _) = True
isLeft _        = False
pickLeft x y = if isLeft x then x else y
fromRight (Right x) = x

tynum = [TYint, TYreal] 

typechk :: Env -> Statement -> Either String Env
typechk env (Assignment (Designator x _) expr) = 
    case gettype env expr of
        Left err -> Left err
        Right v ->
            case (lookup x env)  of 
                Just xtype -> if xtype == v then Right env else
                              Left $ "Can't assign " ++ (show v) ++ "to " ++ (show xtype) 
                _          -> Left $ "Unknown variable " ++ (show x)

typechk env (StatementIf expr s1 ms2) =
    case gettype env expr of
        Left err -> Left err
        Right v ->
            if v /= TYbool 
            then Left "If condition must be boolean" 
            else let tchk1 = typechk env s1
                     tchk2 = case ms2 of Just s2 -> Just $ typechk env s2
                                         Nothing -> Nothing
            in case (tchk1, tchk2) of
                (Left err, _) -> Left err
                (_, Just (Left err)) -> Left err
                _ -> Right env

gettype :: Env -> Expr -> Either String Type
gettype env (FactorInt _ ) = Right TYint
gettype env (FactorReal _) = Right TYreal
gettype env (FactorStr _) = Right TYstr
gettype env FactorTrue = Right TYbool
gettype env FactorFalse = Right TYbool
gettype env (FactorNot x) = undefined
gettype env (FactorFuncCall fc) = undefined

gettype env (FactorDesig (Designator x _)) = 
    case lookup x env of 
        Nothing -> Left $ "Unknown identifier " ++ (show x) 
        Just v  -> Right v

gettype env (Unary op x) = 
    case t of Left _ -> t
              Right TYint -> t
              Right TYreal -> t
              _            -> Left $ (show op) ++ " operand must be integer or real" 
    where t = gettype env x

gettype env (Add x1 op x2)
    | op `elem` [OPplus, OPminus] =
        case (t1, t2) of 
            (Right v1, Right v2) -> 
                if (v1 `notElem` tynum) || (v2 `notElem` tynum)
                then Left $ (show op) ++ " operands must be integer or real"
                else if v1 == TYreal then t1 else t2
            _                    -> pickLeft t1 t2
    | otherwise = 
        case (t1, t2) of
            (Right v1, Right v2) -> 
                if (v1 /= TYbool) || (v2 /= TYbool)
                then Left $ (show op) ++ " operands must be boolean"
                else t1
            _                    -> pickLeft t1 t2
    where t1 = gettype env x1
          t2 = gettype env x2

gettype env (Mult x1 op x2)
    | op `elem` [OPstar, OPdiv] =
        case (t1, t2) of 
            (Right v1, Right v2) -> 
                if (v1 `notElem` tynum) || (v2 `notElem` tynum)
                then Left $ (show op) ++ " operands must be integer or real"
                else if v1 == TYreal then t1 else t2
            _                    -> pickLeft t1 t2
    | otherwise = 
        case (t1, t2) of 
            (Right v1, Right v2) -> 
                if (v1 /= TYbool) || (v2 /= TYbool)
                then Left $ (show op) ++ " operands must be boolean"
                else t1
            _                    -> pickLeft t1 t2
    where t1 = gettype env x1
          t2 = gettype env x2

gettype env (Relation x1 op x2) = -- todo more intricate type comparsions
    case (t1, t2) of 
        (Right v1, Right v2) -> 
            if (v1 == v2) ||
               ((v1 `elem` tynum) && (v2 `elem` tynum)) ||
               ((v1 `elem` tynum) && (v2 `elem` tynum))
            then Right TYbool
            else Left $ (show op) ++ " operands must be boolean"
        _  -> pickLeft t1 t2
    where t1 = gettype env x1
          t2 = gettype env x2

typechkStr s env = case p' parseStatement s of 
        Right st -> typechk env st