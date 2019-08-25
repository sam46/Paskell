{-# LANGUAGE OverloadedStrings #-}

module Grammar where 

import Data.List (find)

data Reserved = KWand | KWdownto | KWif | KWor | KWthen
    | KWarray | KWelse | KWin | KWpacked | KWto | KWbegin
    | KWend | KWlabel | KWprocedure | KWtype | KWcase
    | KWfile | KWmod | KWprogram | KWuntil | KWconst | KWread | KWwrite
    | KWfor | KWnil | KWrecord | KWvar | KWdiv | KWfunction
    | KWnot | KWrepeat | KWwhile | KWdo | KWgoto | KWof | KWforward
    | KWset | KWwith | KWboolean | KWreal | KWinteger | KWdispose
    | KWstring | KWchar | KWnew
    deriving (Show, Eq)

data OP = OPplus | OPminus | OPstar | OPdiv | OPidiv | OPmod
    | OPand | OPeq | OPneq | OPless | OPgreater | OPle | OPge
    | OPin | OPor deriving (Eq)

data Type = TYident Ident | TYbool
    | TYint | TYreal | TYchar | TYstr 
    | TYptr Type | TYarr Int Type | Void
    deriving (Show, Eq, Ord)

type Ident = String 
type IdentList = [Ident]

data Program = Program Ident Block 
             deriving (Show, Eq)
data Block = Block [Decl] Statement
            deriving (Show, Eq)

type VarDecl = (Ident, Type)    -- var a,b:char; 
type TypeDecl = (Ident, Type)   -- type int = integer; 
type CallByRef = Bool

data Decl 
    = DeclVar [VarDecl] 
    | DeclType [TypeDecl] 
    | DeclConst [ConstDecl]
    | DeclForwardProc Ident [(Ident,Type,CallByRef)]
    | DeclForwardFunc Ident [(Ident,Type,CallByRef)] Type
    | DeclProc  Ident [(Ident,Type,CallByRef)] Block
    | DeclFunc Ident [(Ident,Type,CallByRef)] Type Block 
    deriving (Show, Eq)

data ConstDecl = ConstDecl 
                deriving (Show, Eq) -- todo 

data Statement 
    = StatementSeq [Statement] 
    | Assignment Designator Expr
    | ProcCall Ident ExprList
    | StatementIf Expr Statement (Maybe Statement)
    | StatementCase
    | StatementWhile Expr Statement                     -- "while" <expr> "do" <stmt> 
    | StatementRepeat Statement Expr
    | StatementFor Ident Expr ToDownTo Expr Statement
    | StatementNew Ident (Maybe Expr)                   -- "new" [ "[" <expr> "]" ] <l-value>
    | StatementDispose Ident Bool
    | StatementEmpty
    | StatementRead Designator
    | StatementWrite ExprList                           -- "goto" <id> 
    | StatementWriteLn ExprList
    | StatementGoTo Ident 
    deriving (Show, Eq) 

type ToDownTo = Bool

data Designator = Designator Ident [DesigProp]
                deriving (Show, Eq)

data DesigList = DesigList [Designator]
                deriving (Show, Eq)

data DesigProp 
    = DesigPropIdent Ident 
    | DesigPropExprList ExprList 
    | DesigPropPtr
    | DesigPropArr Expr
    deriving (Show, Eq)

type ExprList = [Expr]

data Expr 
    = Relation Expr OP Expr
    | Unary OP Expr
    | Mult Expr OP Expr
    | Add Expr OP Expr
    | FactorInt Int 
    | FactorReal Double 
    | FactorStr String 
    | FactorChar Char
    | FactorTrue 
    | FactorFalse 
    | FactorNil 
    | FactorDesig Designator 
    | FactorNot Expr
    | FuncCall Ident ExprList
    deriving (Show, Eq)

unaryops    = [("+", OPplus), ("-", OPminus)]
addops      = [("+", OPplus), ("-", OPminus), ("or", OPor)]
multops     = [("*", OPstar), ("/", OPdiv), ("div", OPidiv),
                ("mod", OPmod), ("and", OPand)]
relationops = [("=", OPeq), ("<>", OPneq), ("<=", OPle),
                (">=", OPge), ("<", OPless), (">", OPgreater),
                ("in", OPin)]
operators   = addops ++ multops ++ relationops

instance Show OP where
    show op = case find (\(_,b) -> b==op) operators of
                Nothing -> "OP??"
                Just (a,_) -> a