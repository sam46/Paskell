{-# LANGUAGE OverloadedStrings #-}

module Intermediate where 

import Grammar (OP, Type, Ident, IdentList, 
    VarDecl, TypeDecl, CallByRef, ToDownTo) 
import Data.List (intercalate)
import Prelude hiding (showList)

data Program = Program Ident Block Type deriving (Eq)
data Block = Block [Decl] Statement Type deriving (Eq)

data Decl = DeclVar [VarDecl] Type
    | DeclType [TypeDecl]  Type
    | DeclConst [ConstDecl]  Type
    | DeclProc Ident [(Ident,Type,CallByRef)] Block Type
    | DeclFunc Ident [(Ident,Type,CallByRef)] Type Block Type
    deriving (Eq)
data ConstDecl = ConstDecl Type deriving (Show, Eq) -- todo 

data Statement = StatementSeq [Statement] Type
    | Assignment Designator Expr Type
    | ProcCall Ident ExprList Type
    | StatementIf Expr Statement (Maybe Statement) Type
    | StatementCase Type
    | StatementWhile Expr Statement Type
    | StatementRepeat Statement Expr Type
    | StatementFor Ident Expr ToDownTo Expr Statement Type
    | StatementNew Ident Type
    | StatementDispose Ident Type
    | StatementEmpty
    | StatmentRead DesigList Type
    | StatementReadLn DesigList Type
    | StatementWrite ExprList Type
    deriving (Eq) 

data Designator = Designator Ident [DesigProp] Type deriving (Eq)
data DesigList = DesigList [Designator] Type deriving (Show, Eq)
data DesigProp = DesigPropIdent Ident  Type
    | DesigPropExprList ExprList  Type
    | DesigPropPtr  Type
    deriving (Show, Eq)

type ExprList = [Expr]
data Expr = Relation Expr OP Expr Type
    | Unary OP Expr Type
    | Mult Expr OP Expr Type
    | Add Expr OP Expr Type
    | FactorInt Int  Type
    | FactorReal Double  Type
    | FactorStr String  Type
    | FactorTrue  Type
    | FactorFalse  Type
    | FactorNil  Type
    | FactorDesig Designator  Type
    | FactorNot Expr Type
    | FuncCall Ident ExprList Type
    deriving (Eq)

getType :: Expr -> Type
getType (Relation _ _ _ t) = t
getType (Unary _ _ t) = t
getType (Mult _ _ _ t) = t
getType (Add  _ _ _ t) = t
getType (FactorInt _  t) = t
getType (FactorReal _  t) = t
getType (FactorStr _  t) = t
getType (FactorTrue  t) = t
getType (FactorFalse  t) = t
getType (FactorNil  t) = t
getType (FactorDesig _  t) = t
getType (FactorNot _ t) = t
getType (FuncCall _ _ t) = t

instance Ord Expr where
    x1 `compare` x2 = t1 `compare` t2
        where (t1, t2) = (getType x1, getType x2) 

--------- Pretty-Printer ---------

showList xs = intercalate " " (map show xs)
showListSep xs = intercalate ", " (map show xs)
showListNoSpc xs = intercalate "" (map show xs)
tab n = intercalate "" (replicate n "    ")

instance Show Program where
    show (Program x b _) = "Program "++  x ++ " " ++ (pshowB 0 b)

instance Show Block where
    show x = pshowB 0 x

pshowB n (Block ds s _) = "{\n"++ (if length ds == 0 then "" 
    else (intercalate "" $ map (pshowD (n+1)) ds)) ++ "\n" ++  (pshowSt (n+1) s) ++ "\n"
        ++ (tab n) ++ "}"

instance Show Decl where
    show x = pshowD 0 x

pshowD n (DeclVar xs _) = (tab n) ++ "Var " ++ (if length xs == 0 then "" 
    else showList xs) ++ ";\n"
pshowD n (DeclType xs _) = (tab n) ++ "Type " ++ (if length xs == 0 then "" 
    else showList xs) ++ ";\n"
pshowD n (DeclConst xs _) = (tab n) ++ "Const " ++ (if length xs == 0 then "" 
    else showList xs) ++ ";\n"
pshowD n (DeclProc x xs b _) = (tab n) ++ "Proc " ++  x ++ " " ++ 
    "(" ++ showListSep (map (\(a',b',_) -> (a',b')) xs) 
    ++ ") " ++ (pshowB n b) ++ "\n"
pshowD n (DeclFunc x xs t b _) = (tab n) ++ "Func " ++ x ++ ":" ++ (show t) ++ " " ++ 
    "(" ++ showListSep (map (\(a',b',_) -> (a',b')) xs) 
    ++ ") " ++ (pshowB n b) ++ "\n"

instance Show Designator where
    show x = pshowDes 0 x

pshowDes n (Designator x _ t) = (tab n) ++ x ++ ":" ++ (show t)

instance Show Statement where
    show x = pshowSt 0 x
pshowSt n (Assignment x ex t) = (pshowDes n x) ++ " := " ++ (pshowEx 0 ex) ++ ";\n"
pshowSt n (StatementEmpty) = (tab n) ++ ";\n"
pshowSt n (StatementSeq xs _) = if length xs == 0 then "" 
    else (intercalate "" $ map (pshowSt n) xs)
pshowSt n (StatementIf ex s ms _) =  (tab n) ++  "if " ++ (pshowEx 0 ex) ++"\n"++ 
    (tab n) ++ "then\n" ++ (pshowSt (n+1) s) 
    ++ (case ms of Nothing -> ""
                   Just s2 -> (tab n) ++ "else\n" ++ (pshowSt (n+1) s2))
pshowSt n _ = (tab n) ++ "??" ++ ";\n"

instance Show Expr where
    show x = pshowEx 0 x

pshowEx n (Relation ex1 op ex2 t) =  "("++(pshowEx 0 ex1) ++ (show op) ++ (pshowEx 0 ex2) ++ "):" ++ (show t) 
pshowEx n (Unary op ex t) = "(" ++(show op) ++ (pshowEx 0 ex) ++ "):" ++ (show t) 
pshowEx n (Mult ex1 op ex2 t) = "("++(pshowEx 0 ex1) ++ (show op) ++ (pshowEx 0 ex2) ++ "):" ++ (show t) 
pshowEx n (Add ex1 op ex2 t) = "("++(pshowEx 0 ex1) ++ (show op) ++ (pshowEx 0 ex2) ++ "):" ++ (show t) 
pshowEx n (FactorInt x  t) = (show x) ++ ":" ++ (show t) 
pshowEx n (FactorReal x  t) = (show x) ++ ":" ++ (show t) 
pshowEx n (FactorStr x  t) = (show x) ++ ":" ++ (show t) 
pshowEx n (FactorTrue  t) = "True" ++ ":" ++ (show t) 
pshowEx n (FactorFalse  t) = "False" ++ ":" ++ (show t) 
pshowEx n (FactorNil  t) = "Nil" ++ ":" ++ (show t) 
pshowEx n (FactorDesig x t) = (pshowDes 0 x) ++ ":" ++ (show t) 
pshowEx n (FactorNot ex t) = undefined
pshowEx n (FuncCall x exs t) = x ++ "(" ++ 
    (if length exs == 0 then "" else showListSep exs) ++"):"++(show t)
