module TypeCheck where

import Grammar
import Paskell 
import Utils (p')
import Data.List
import Control.Monad (msum)

-- Environment is a Func/Proc signatures + stack of Contexts
type Env = (Sig, [Context], [TContext]) 
-- Function sig is return type + formal args types
type Sig = [(Ident, (Type, [(Type, CallByRef)]))]
type Context = [(Ident, Type)]
type TContext = [(Ident, Type)]

-- | Errors
data TyErr 
    = NotInScope Ident
    | TypeMismatch Type Type 
    | TypeMismatchOrd Type
    | TypeMismatchNum Type
    | ArgCountMismatch Int Int
    | ArgTypeMismatch Type Type
    | CondTypeMismatch Type
    | VarRedecl Ident
    | FuncRedecl Ident
    | VaraibleArgExpected Expr
    | TypeRedecl Ident
    | UnknownType Ident 
    deriving (Show, Eq)

-- | Returns True if Variable is in Context. False otherwise
varInContext :: Context -> Ident -> Bool
varInContext ctx x = case (lookup x ctx) of 
    Nothing -> False
    Just _  -> True

-- | Returns True if identifier has known type in Context. False otherwise
typeInContext :: TContext -> Ident -> Bool
typeInContext tctx x = case (lookup x tctx) of 
    Nothing -> False
    Just _  -> True

-- | Lookup Variable in all Contexts
lookupVar :: Env -> Ident -> Either TyErr Type
lookupVar (_, contexts, _) x = case (find (`varInContext` x) contexts) of
                    Nothing  -> Left  $ NotInScope x
                    Just ctx -> case lookup x ctx of
                        Nothing  -> Left  $ NotInScope x
                        Just t   -> Right t

-- | Lookup Function in Context
lookupFun :: Env -> Ident -> Either TyErr (Type, [(Type,CallByRef)])
lookupFun (sigs, _, _) x = 
    case lookup x sigs of
        Nothing -> Left $ NotInScope x
        Just f  -> Right f
    
lookupType :: Env -> Type -> Either TyErr Type
lookupType e@(_, _, contexts) (TYident x) = case (find (`typeInContext` x) contexts) of
                    Nothing  -> Left $ UnknownType x
                    Just ctx -> case lookup x ctx of
                        Nothing  -> Left $ UnknownType x
                        Just t   -> lookupType e t
lookupType _ t = Right t

-- eqType env t1 t2 = (lookupType env t1) == (lookupType env t2)

-- | Appends a new block in the Environment
newBlock  :: Env -> Env
newBlock (sig, ctx, tctx) = (sig, [] : ctx, [] : tctx)

-- | Initial Empty Environment
emptyEnv  :: Env
emptyEnv = ([], [], [])

-- | Get Function Signature
getSig :: Decl -> (Ident, (Type, [(Type,CallByRef)]))
getSig (DeclFunc x args t _) = (x, (t, map (\(_,b,c) -> (b,c)) args))

-- updateVar :: Env -> Ident -> Type -> Either TyErr Env

-- | Add Variable in Context. If its already in Context returns error
addVar :: Env -> Ident -> Type -> Either TyErr Env
addVar (sig, (c:cs), tctx) x t = if varInContext c x
    then Left $ VarRedecl x
    else Right $ (sig, ((x,t):c) : cs, tctx)

-- | Add Function in Environment. Error if it is already declared
addFunc :: Env -> (Ident, (Type, [(Type,CallByRef)])) -> Either TyErr Env
addFunc (sigs, ctx, tctx) (x, rest) = 
    case lookup x sigs of
        Just _   -> Left $ FuncRedecl x
        Nothing  -> Right ((x, rest) : sigs, ctx, tctx)

addType :: Env -> Ident -> Type -> Either TyErr Env
addType (sig, ctx, (c:cs)) x t = if typeInContext c x
    then Left $ TypeRedecl x
    else Right $ (sig, ctx, ((x,t):c) : cs)
addType env x t = error $ (show env) ++ (show x) ++ (show t)

-- isLeft :: (Either a b) -> Bool 
-- isLeft (Left _) = True
-- isLeft _        = False
-- pickLeft x y = if isLeft x then x else y
-- fromRight (Right x) = x

isNum :: Type -> Bool
isNum (TYarr _ TYint) = True
isNum (TYarr _ TYreal) = True
isNum x = x `elem` [TYint, TYreal] 

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
    lookupType env t >>= \r ->
    (addVar env x r) >>= \e -> typechkDecl e (DeclVar ds) 
typechkDecl env (DeclFunc x params t b) = lookupType env t >>= \r ->
    typechkDeclFunc env (DeclFunc x params' r b)
    where params' = (x,t,False) : params -- added hidden variable for return value dummy
typechkDecl env (DeclProc x params b) = typechkDeclFunc env (DeclFunc x params Void b)
typechkDecl env (DeclType []) = Right env
typechkDecl env (DeclType (t:ts)) = let (x,ty) = t in
    (addType env x ty) >>= \e -> typechkDecl e (DeclType ts) 

resolveParamsType :: Env -> [(Ident,Type,CallByRef)] -> Either TyErr [(Ident,Type,CallByRef)]
resolveParamsType env params =
    mapM (\(x,t,b) -> lookupType env t >>= \r -> Right (x,r,b)) params 

typechkConstDecl :: Env -> ConstDecl -> Either TyErr Env
typechkConstDecl env _ = error $ "typechkConstDecl: undefined"

typechkDeclFunc :: Env -> Decl -> Either TyErr Env
typechkDeclFunc env (DeclFunc x params t b) =
    resolveParamsType env params >>= \resParams -> -- replace params type aliases with their real types
    let sig = getSig (DeclFunc x resParams t b) in
    (addFunc env sig) >>= \e ->
        typechkDecls (newBlock e) (map (\(i,ty,_) -> DeclVar [(i, ty)]) resParams) >>= \e2 ->
            typechkBlock e2 b >> Right e


typechkStatement :: Env -> Statement -> Either TyErr Env
typechkStatement env (Assignment (Designator x _) expr) = -- todo: assignment to array
    gettype env expr >>= \t -> lookupVar env x >>= \xtype -> do
        if xtype == t                      
           || (xtype == TYstr && t == TYchar)
           || (xtype == TYreal && t == TYint)
        then Right env
        else if (isArray xtype && getArrType xtype == t)
            then Right env
        else Left $ TypeMismatch xtype t
        where
            isArray :: Type -> Bool
            isArray (TYarr _ _) = True
            isArray _ = False
            -- | Get array type
            getArrType :: Type -> Type
            getArrType (TYarr _ ty) = getArrType ty
            getArrType (TYptr ty) = getArrType ty
            getArrType ty = ty

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

typechkStatement env (ProcCall x args) = lookupFun env x >>=
    \(t, formalTs) -> 
        if (length formalTs) /= (length args)
        then Left $ ArgCountMismatch (length formalTs) (length args)
        else case foldr (>>) (Right t) (zipWith (matchArgFormal env) args formalTs)
             of Right _ -> Right env
                Left err -> Left err

typechkStatement env _ = Right env -- todo

-- check if argument matches expected formal parameter
matchArgFormal :: Env -> Expr -> (Type, Bool) -> Either TyErr Type
matchArgFormal env expr (ty, callbyref) = gettype env expr >>= \exprT -> 
    if callbyref 
    then -- has to be a Designator of the exact same type
        if   exprT /= ty 
        then Left $ ArgTypeMismatch ty exprT
        else if (not $ isFactorDesig expr) && callbyref -- a CallByRef argument has to be a FactorDesig 
        then Left $ VaraibleArgExpected expr
        else Right ty
    else -- typecasts
        if   ty == exprT
             || (ty == TYreal && exprT == TYint)
             || (ty == TYstr  && exprT == TYchar)
        then Right ty
        else Left $ ArgTypeMismatch ty exprT
    where isFactorDesig a = case a of
            FactorDesig _ -> True
            _             -> False

tail' [] = []
tail' xs = tail xs

gettype :: Env -> Expr -> Either TyErr Type
gettype env FactorTrue          = Right TYbool
gettype env FactorFalse         = Right TYbool
gettype env (FactorInt _)       = Right TYint
gettype env (FactorReal _)      = Right TYreal
gettype env (FactorStr _)       = Right TYstr
gettype env (FactorChar _)      = Right TYchar
gettype env (FactorNot x)       = undefined

-- error here: no args
gettype env (FuncCall x args) = lookupFun env x >>=
    \(t, f : formalTs) -> 
        -- discard the dummy formal parameter
        if (length formalTs) /= (length args)
        then Left $ ArgCountMismatch (length formalTs) (length args)
        else foldr (>>) (Right t) (zipWith (matchArgFormal env) args formalTs)

-- array access

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
    | op `elem` [OPplus, OPminus] = -- +|-
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
    | op == OPstar =    -- *
        t1 >>= \v1 -> t2 >>= \v2 ->
            if not (isNum v1 && isNum v2)
            then Left $ TypeMismatchNum (if isNum v1 then v1 else v2)
            else if v1 == TYreal then t1 else t2
    | op == OPdiv =     -- /
        t1 >>= \v1 -> t2 >>= \v2 ->
            if not (isNum v1 && isNum v2)
            then Left $ TypeMismatchNum (if isNum v1 then v1 else v2)
            else Right TYreal
    | op `elem` [OPmod, OPidiv] =   -- mod, div
        t1 >>= \v1 -> t2 >>= \v2 ->
            if not (v1 == TYint && v2 == TYint)
            then Left $ TypeMismatch TYint (if v1 == TYint then v1 else v2)
            else Right TYint
    | otherwise = 
        t1 >>= \v1 -> t2 >>= \v2 ->
            if (v1 /= TYbool) || (v2 /= TYbool)
            then Left $ TypeMismatch TYbool (if v1 /= TYbool then v1 else v2)
            else t1
    where [t1, t2] = (gettype env) <$> [x1, x2]

typechkStr s env = case p' parseStatement s of 
    Right st -> typechkStatement env st
    Left err -> error $ "Parse error:\n\t" ++ s ++ "\nin:" ++ show err
