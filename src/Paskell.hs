module Paskell where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Utils (parse')

data Reserved = KWvar | KWtype | KWconst | KWbegin | KWend |
    KWprogram deriving (Show)

data Ident = Ident String deriving (Show)
data Type = TYident Ident | TYchar | TYboolean |
    TYinteger | TYreal | TYstring deriving (Show)
data IdentList = IdentList [Ident]     deriving (Show)

data Decl = DeclVar [VarDecl] | DeclType [TypeDecl] |
    DeclConst [ConstDecl] deriving (Show)
data VarDecl = VarDecl IdentList Type     deriving (Show)
data TypeDecl = TypeDecl Ident Type     deriving (Show)
data ConstDecl = ConstDecl deriving (Show) -- todo 

data Program = Program Ident Block deriving (Show)
data Block = Block [Decl] [Statement] deriving (Show)

data Statement = Statement deriving (Show) -- todo

------------------------------------------------------

comments :: Parser ()
comments = spaces -- todo change to actual comment parser

whitespace :: Parser ()
whitespace = do {spaces; comments; spaces; comments}

tok :: Parser a -> Parser a
tok p = p >>= \x -> spaces >>= \_ -> return x 

charTok :: Char -> Parser Char
charTok = tok . char  

commaTok :: Parser Char
commaTok = charTok ','

semicolTok :: Parser Char
semicolTok = charTok ';'

charIgnoreCaseTok :: Char -> Parser Char
charIgnoreCaseTok = charTok (toUpper c) <|> charTok (toLower c)

stringIgnoreCaseTok :: String -> Parser String
stringIgnoreCaseTok = 

charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = char (toUpper c) <|> char (toLower c)

stringIgnoreCase :: String -> Parser String
stringIgnoreCase [] = return []
stringIgnoreCase (x:xs) = do {c <- charIgnoreCase x; cs <- stringIgnoreCase xs; return $ [c] ++ cs}




------------------------------------------------------
reserved = ["var", "char"]

parseReserved :: Parser Reserved
parseReserved = undefined -- todo

parseIdent :: Parser Ident
parseIdent = fff -- <?> "reserved kw " --  <|> (unexpected $ "error: " ++ " is a reserved keyword")
    where fff = do
                l <- letter
                rest <- many alphaNum
                if not (elem ([l] ++ rest) reserved) then return $ Ident $ [l] ++ rest else parserZero
    -- todo exclude Reserved keywords



parserType :: Parser Type
parserType = 
    (stringIgnoreCase "boolean" >> return TYboolean) <|>
    (stringIgnoreCase "integer" >> return TYinteger) <|>
    (stringIgnoreCase "real" >> return TYreal) <|>
    (stringIgnoreCase "char" >> return TYchar) <|>
    (stringIgnoreCase "string" >> return TYstring) <|>
    (parseIdent >>= \x -> return $ TYident x)

parseIdentList :: Parser IdentList
parseIdentList = IdentList <$> sepBy1 parseIdent sep
    where sep = try (spaces >> char ',' >> spaces)

parseVarDecl :: Parser [VarDecl]
parseVarDecl = do
    try $ between spaces (many1 space) (stringIgnoreCase "var")
    many1 $ do
        -- spaces
        idents <- parseIdent -- parseIdentList
        between spaces spaces (char ':')
        t <- parserType
        -- between spaces spaces (char ';')
        spaces
        char ';'
        return $ VarDecl (IdentList [idents]) t

parseTypeDecl :: Parser [TypeDecl]
parseTypeDecl = do
    between spaces (many1 space) (stringIgnoreCase "type")
    many1 $ do
        spaces
        ident <- parseIdent
        between spaces spaces (char '=')
        t <- parserType
        -- between spaces spaces (char ';')
        spaces 
        (char ';')
        return $ TypeDecl ident t

parseConstDecl :: Parser [ConstDecl]
parseConstDecl = undefined -- todo


parseProgram :: Parser Program
parseProgram = do
    between spaces (many1 space) (stringIgnoreCase "program")
    ident <- parseIdent
    between spaces spaces (char ';')
    block <- parseBlock
    between spaces spaces (char '.')
    -- try (spaces >> char '.')
    -- try (char '.')
    return $ Program ident block

parseBlock :: Parser Block
parseBlock = do
    decls <- between spaces spaces parseDecl
    return $ Block decls [] -- todo [Statements]


parseDecl :: Parser [Decl]
parseDecl = many $
    -- (parseTypeDecl >>= \xs -> return $ DeclType xs) <|>
    (parseVarDecl >>= \xs -> return $ DeclVar xs) -- <|>
    -- (parseConstDecl >>= \xs -> return $ DeclConst xs)


parserStatement :: Parser Statement
parserStatement = undefined -- todo