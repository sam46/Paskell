module Grammar where 

-- Reserved keywords
data Reserved = KWand | KWdownto | KWif | KWor | KWthen | 
    KWarray | KWelse | KWin | KWpacked | KWto | KWbegin | 
    KWend | KWlabel | KWprocedure | KWtype | KWcase | 
    KWfile | KWmod | KWprogram | KWuntil | KWconst | 
    KWfor | KWnil | KWrecord | KWvar | KWdiv | KWfunction |
    KWnot | KWrepeat | KWwhile | KWdo | KWgoto | KWof | 
    KWset | KWwith deriving (Show)

data Type = TYident Ident | TYchar | TYboolean |
    TYinteger | TYreal | TYstring deriving (Show)

-- Identifier
data Ident = Ident String deriving (Show)
data IdentList = IdentList [Ident]     deriving (Show)


-- Program module and block
data Program = Program Ident Block deriving (Show)
data Block = Block [Decl] [Statement] deriving (Show)

-- Declarations
data Decl = DeclVar [VarDecl] | DeclType [TypeDecl] |
    DeclConst [ConstDecl] deriving (Show)
data VarDecl = VarDecl IdentList Type     deriving (Show)
data TypeDecl = TypeDecl IdentList Type     deriving (Show)
data ConstDecl = ConstDecl deriving (Show) -- todo 

data Statement = Statement deriving (Show) -- todo
