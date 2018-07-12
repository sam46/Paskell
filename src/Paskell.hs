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
    (parseIdent >>= \x -> return $ TYident x) <|>
    (stringIgnoreCase "boolean" >> return TYboolean) <|>
    (stringIgnoreCase "integer" >> return TYinteger) <|>
    (stringIgnoreCase "real" >> return TYreal) <|>
    (stringIgnoreCase "char" >> return TYchar) <|>
    (stringIgnoreCase "string" >> return TYstring)

parseIdentList :: Parser IdentList
parseIdentList = IdentList <$> sepBy1 parseIdent commaTok

parseVarDecl :: Parser [VarDecl]
parseVarDecl = (parseKWvar <?> "expecting keyword 'var'") 
    >> ((many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
            (do {l <- parseIdentList; charTok ':'; t <- parserType; semicolTok; return $ VarDecl l t})
        ) <?> "Missing or incorrect variable declaration")

parseTypeDecl :: Parser [TypeDecl]
parseTypeDecl = (parseKWtype <?> "expecting keyword 'type'") 
    >> ((many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
            (do {l <- parseIdentList; charTok '='; t <- parserType; semicolTok; return $ TypeDecl l t})
        ) <?> "Missing or incorrect type declaration")

parseConstDecl :: Parser [ConstDecl]
parseConstDecl = undefined -- todo


parseProgram :: Parser Program
parseProgram = spaces >> between parseKWprogram (charTok '.') 
    (do prog <- parseIdent
        semicolTok
        blk <- parseBlock
        return $ Program prog blk)

parseBlock :: Parser Block
parseBlock = do
    decls <- many parseDecl
    -- todo StatementList [...]
    return $ Block decls (StatementList [{- todo -}])


parseDecl :: Parser Decl
parseDecl = 
    (parseTypeDecl >>= \xs -> return $ DeclType xs) <|>
    (parseVarDecl >>= \xs -> return $ DeclVar xs) -- <|>
    -- (parseConstDecl >>= \xs -> return $ DeclConst xs)


parserStatementList :: Parser StatementList
parserStatementList = undefined -- todo
parserStatement :: Parser Statement
parserStatement = undefined -- todo


parseOP :: Parser OP
parseOP = let f (x, y) = try (stringTok x >> return y) in 
    foldr (<|>) (fail "Expecting operator") (map f operators)
