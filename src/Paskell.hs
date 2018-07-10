module Paskell where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
-- import our own modules
import Grammar
import ExtraParsers
import KeywordParse
import Utils (p')

------------------------------------------------------
-- make sure keywords are lower-cased
keywords = ["and","downto","if","or",
    "then","array","else","in","packed",
    "to","begin","end","label","procedure",
    "type","case","file","mod","program","until",
    "const","for","nil","record","var",
    "div","function","not","repeat","while",
    "do","goto","of","set","with"]
special = [":=","+","-","*","/","="
    ,"<",">","<>","<=",">=","(",")","[",
    "]",",",".",";",":","..","^"]



parseIdent :: Parser Ident
parseIdent = tok . try $ do
    x <- letter
    xs <- many alphaNum
    let ident = x:xs
    if (toLower <$> ident) `elem` keywords 
        then fail ("Expecting identifier but found keyword " ++ ident)
        else return (Ident ident) 

parserType :: Parser Type
parserType = tok $ 
    (stringIgnoreCase "boolean" >> return TYboolean) <|>
    (stringIgnoreCase "integer" >> return TYinteger) <|>
    (stringIgnoreCase "real" >> return TYreal) <|>
    (stringIgnoreCase "char" >> return TYchar) <|>
    (stringIgnoreCase "string" >> return TYstring) <|>
    (parseIdent >>= \x -> return $ TYident x)

parseIdentList :: Parser IdentList
parseIdentList = IdentList <$> sepBy1 parseIdent commaTok

parseVarDecl :: Parser [VarDecl]
parseVarDecl = (parseKWvar <?> "expecting keyword 'var'") 
    >> ((many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {l <- parseIdentList; charTok ':'; t <- parserType; semicolTok; return $ VarDecl l t})
        ) <?> "Missing or incorrect variable declaration")


-- parseTypeDecl :: Parser [TypeDecl]
-- parseTypeDecl = do
--     between spaces (many1 space) (stringIgnoreCase "type")
--     many1 $ do
--         spaces
--         ident <- parseIdent
--         between spaces spaces (char '=')
--         t <- parserType
--         -- between spaces spaces (char ';')
--         spaces 
--         (char ';')
--         return $ TypeDecl ident t

-- parseConstDecl :: Parser [ConstDecl]
-- parseConstDecl = undefined -- todo


-- parseProgram :: Parser Program
-- parseProgram = do
--     between spaces (many1 space) (stringIgnoreCase "program")
--     ident <- parseIdent
--     between spaces spaces (char ';')
--     block <- parseBlock
--     between spaces spaces (char '.')
--     -- try (spaces >> char '.')
--     -- try (char '.')
--     return $ Program ident block

-- parseBlock :: Parser Block
-- parseBlock = do
--     decls <- between spaces spaces parseDecl
--     return $ Block decls [] -- todo [Statements]


-- parseDecl :: Parser [Decl]
-- parseDecl = many $
--     -- (parseTypeDecl >>= \xs -> return $ DeclType xs) <|>
--     (parseVarDecl >>= \xs -> return $ DeclVar xs) -- <|>
--     -- (parseConstDecl >>= \xs -> return $ DeclConst xs)


-- parserStatement :: Parser Statement
-- parserStatement = undefined -- todo