module Intermediate where 

import Grammar (OP, Type, Ident, IdentList, VarDecl, TypeDecl, ToDownTo) 

data Program = Program Ident Block Type deriving (Show, Eq)
data Block = Block [Decl] Statement Type deriving (Show, Eq)

data Decl = DeclVar [VarDecl] Type
    | DeclType [TypeDecl]  Type
    | DeclConst [ConstDecl]  Type
    | DeclProc Ident [(Ident,Type,Bool)] Block Type
    | DeclFunc Ident [(Ident,Type,Bool)] Type Block Type
    deriving (Show, Eq)
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
    | StatementWriteLn ExprList Type
    deriving (Show, Eq) 

data Designator = Designator Ident [DesigProp] Type deriving (Show, Eq)
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
    deriving (Show, Eq)

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
