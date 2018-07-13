module Paskell where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Grammar
import ExtraParsers
import KeywordParse
import Utils (p')


parseIdent :: Parser Ident
parseIdent = tok . try $ do
    x  <- letter
    xs <- many alphaNum
    let ident = x:xs
    if (toLower <$> ident) `elem` keywords 
    then fail ("Expecting identifier but found keyword " ++ ident)
    else return (Ident ident)

parserType :: Parser Type
parserType = tok $ 
    (parseIdent >>= \x -> return $ TYident x)        <|>
    (stringIgnoreCase "boolean" >> return TYboolean) <|>
    (stringIgnoreCase "integer" >> return TYinteger) <|>
    (stringIgnoreCase "real"    >> return TYreal)    <|>
    (stringIgnoreCase "char"    >> return TYchar)    <|>
    (stringIgnoreCase "string"  >> return TYstring)

parseIdentList :: Parser IdentList
parseIdentList = IdentList <$> sepBy1 parseIdent commaTok

parseVarDecl :: Parser [VarDecl]
parseVarDecl = (parseKWvar <?> "expecting keyword 'var'") >>
    ((many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {l <- parseIdentList; charTok ':';
                t <- parserType; semicolTok; return $ VarDecl l t})
        ) <?> "Missing or incorrect variable declaration")

parseTypeDecl :: Parser [TypeDecl]
parseTypeDecl = (parseKWtype <?> "expecting keyword 'type'") >> 
    ((many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {l <- parseIdentList; charTok '=';
                t <- parserType; semicolTok; return $ TypeDecl l t})
        ) <?> "Missing or incorrect type declaration")

parseConstDecl :: Parser [ConstDecl]
parseConstDecl = undefined -- todo


parseProgram :: Parser Program
parseProgram = spaces >> between parseKWprogram (charTok '.') 
    (do prog <- parseIdent
        semicolTok
        blok <- parseBlock
        return $ Program prog blok)

parseBlock :: Parser Block
parseBlock = do
    decls <- many parseDecl
    -- todo StatementList [...]
    return $ Block decls (StatementList [{- todo -}])


parseDecl :: Parser Decl
parseDecl = 
    (parseTypeDecl >>= \xs -> return $ DeclType xs) <|>
    (parseVarDecl  >>= \xs -> return $ DeclVar xs) -- <|>
    -- (parseConstDecl >>= \xs -> return $ DeclConst xs)


makeOPparser :: [(String, OP)] -> Parser OP
makeOPparser xs = let f (a, b) = try (stringTok a >> return b) 
    in foldr (<|>) (fail "Expecting operator") (map f xs)
parseOP         = makeOPparser operators -- any OP (relation, additive, mult, unary)
parseOPunary    = OPunary    <$> makeOPparser unaryops
parseOPadd      = OPadd      <$> makeOPparser addops
parseOPmult     = OPmult     <$> makeOPparser multops
parseOPrelation = OPrelation <$> makeOPparser relationops

parseDesignator :: Parser Designator
parseDesignator = parseIdent 
    >>= \x -> try (many parseDesigProp)
    >>= \y -> return $ Designator x y

parseDesigProp :: Parser DesigProp
parseDesigProp =
    (charTok '.' >> DesigPropIdent <$> parseIdent) <|>
    (charTok '^' >> return DesigPropPtr)           <|>
    (DesigPropExprList <$> betweenCharTok 
        '[' ']' parseExprList)

parseDesigList :: Parser DesigList
parseDesigList = DesigList <$> many1 parseDesignator

parseExpr :: Parser Expr
parseExpr = undefined

parseExprList :: Parser ExprList -- non-empty
parseExprList = undefined

parseTerm :: Parser Term
parseTerm = parseFactor 
    >>= \x  -> (try (many ((,) <$> parseOPmult <*> parseFactor)) <|> (return []))
    >>= \xs -> return $ uncurry (Term x) (unzip xs)
    --     do
    -- opmult <- parseOPmult
    -- fact   <- parseFactor
    -- return (opmult, fact)) 
    -- >>

parseFactor :: Parser Factor
parseFactor = 
        (parseKWnil >> return FactorNil)
    <|> (parseKWnot >> FactorNot <$> parseFactor)
    <|> (exactTok "true"  >> return FactorTrue)
    <|> (exactTok "false" >> return FactorFalse) 
    <|> (FactorStr <$> parseString)
    <|> (FactorExpr <$> (betweenCharTok '[' ']' parseExpr))
    <|> (FactorDesig <$> parseDesignator)
    <|> (FactorFuncCall <$> parseFuncCall)

parseStmntList :: Parser StatementList -- non-empty
parseStmntList = undefined

parseStatement :: Parser Statement 
parseStatement = undefined


parseIf :: Parser Statement
parseIf = undefined

parseCase :: Parser Statement
parseCase = undefined

parseRepeat :: Parser Statement
parseRepeat = undefined

parseWhile :: Parser Statement
parseWhile = undefined

parseFor :: Parser Statement
parseFor = undefined

parseMem :: Parser Mem
parseMem = undefined

parseAssignment :: Parser Statement
parseAssignment = undefined

parseProcCall :: Parser Statement
parseProcCall = undefined

parseStmntMem :: Parser Statement
parseStmntMem = undefined

parseStmntIO :: Parser Statement
parseStmntIO = undefined

parseFuncCall :: Parser FuncCall
parseFuncCall = fail ""


parseString :: Parser String
parseString = between (char '"') (charTok '"') $ many $
    (noneOf ['\\', '"']) <|>
    ((char '\\') >> anyChar >>= \c -> case toSpecialChar c 
    of Just x  -> return (fromSpecialChar x)
       Nothing -> if c == 'u' then undefined  -- todo hex
                  else unexpected ("char in string " ++ [c]))
