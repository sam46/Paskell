module Grammar where 

data Reserved = KWand | KWdownto | KWif | KWor | KWthen | 
    KWarray | KWelse | KWin | KWpacked | KWto | KWbegin | 
    KWend | KWlabel | KWprocedure | KWtype | KWcase | 
    KWfile | KWmod | KWprogram | KWuntil | KWconst | 
    KWfor | KWnil | KWrecord | KWvar | KWdiv | KWfunction |
    KWnot | KWrepeat | KWwhile | KWdo | KWgoto | KWof | 
    KWset | KWwith | KWboolean | KWreal | KWinteger |
    KWstring | KWchar deriving (Show, Eq)

data OP = OPplus | OPminus | OPstar | OPdiv | OPidiv | OPmod | 
    OPand | OPeq | OPneq | OPless | OPgreater | OPle | OPge | 
    OPin | OPor deriving (Show, Eq)
-- data OPunary = OPunary OP deriving (Show)
-- data OPadd = OPadd OP deriving (Show)
-- data OPmult = OPmult OP deriving (Show)
-- data OPrelation = OPrelation OP deriving (Show)
-- data Number = NUMint Int | NUMreal Double deriving (Show)

data Type = TYident Ident | TYchar | TYbool |
    TYint | TYreal | TYstr deriving (Show, Eq)
type Ident = String 
type IdentList = [Ident]

data Program = Program Ident Block deriving (Show, Eq)
data Block = Block [Decl] Statement deriving (Show, Eq)

data Decl = DeclVar [VarDecl] 
    | DeclType [TypeDecl] 
    | DeclConst [ConstDecl] 
    | DeclProc ProcDecl
    | DeclFunc FuncDecl 
    deriving (Show, Eq)
data VarDecl = VarDecl IdentList Type deriving (Show, Eq)
data TypeDecl = TypeDecl IdentList Type deriving (Show, Eq)
data ConstDecl = ConstDecl deriving (Show, Eq) -- todo 

data ProcDecl = ProcDecl Ident [(Ident,Type,Bool)] Block deriving (Show, Eq) 
data FuncDecl = FuncDecl Ident [(Ident,Type,Bool)] Type Block deriving (Show, Eq) 

data Statement = StatementSeq [Statement]  |
    Assignment Designator Expr |
    ProcCall Ident ExprList |
    StatementIf Expr Statement (Maybe Statement) |
    StatementCase | -- todo
    StatementWhile Expr Statement |
    StatementRepeat Statement Expr |
    StatementFor Ident Expr ToDownTo Expr Statement |
    StatementIO StatementIO |
    StatementMem Mem Ident |
    StatementEmpty  deriving (Show, Eq)
data StatementIO = StatmentRead DesigList | StatementReadLn DesigList |
    StatementWrite ExprList | StatementWriteLn ExprList deriving (Show, Eq) 
data Mem = MemNew | MemDispose deriving (Show, Eq) 
type ToDownTo = Bool

data Designator = Designator Ident [DesigProp] deriving (Show, Eq)
data DesigList = DesigList [Designator] deriving (Show, Eq)
data DesigProp = DesigPropIdent Ident | DesigPropExprList ExprList | 
    DesigPropPtr deriving (Show, Eq)

type ExprList = [Expr]
data Expr = Relation Expr OP Expr
    | Unary OP Expr
    | Mult Expr OP Expr
    | Add Expr OP Expr
    | FactorInt Int | FactorReal Double 
    | FactorStr String 
    | FactorTrue | FactorFalse 
    | FactorNil 
    | FactorDesig Designator 
    | FactorNot Expr
    | FuncCall Ident ExprList
    deriving (Show, Eq)
