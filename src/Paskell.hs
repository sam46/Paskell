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
data Decl = DeclVar [VarDecl] | DeclType [TypeDecl]  deriving (Show)
data VarDecl = VarDecl IdentList Type     deriving (Show)
data TypeDecl = TypeDecl IdentList Type     deriving (Show)

parseReserved :: Parser Reserved
parseReserved = undefined -- todo

parseIdent :: Parser Ident
parseIdent = do
    l <- letter
    rest <- many alphaNum
    return $ Ident $ [l] ++ rest
    -- todo exclude Reserved keywords

charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = char (toUpper c) <|> char (toLower c)

stringIgnoreCase :: String -> Parser String
stringIgnoreCase [] = return []
stringIgnoreCase (x:xs) = do
    c <- charIgnoreCase x
    cs <- stringIgnoreCase xs
    return $ [c] ++ cs

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
    between spaces (many1 space) (stringIgnoreCase "var")
    many1 $ do
        idents <- parseIdentList
        between spaces spaces (char ':')
        t <- parserType
        between spaces spaces (char ';')
        return $ VarDecl idents t
    

-- parseDecl :: Parser Decl
