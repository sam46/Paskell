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

parseType :: Parser Type
parseType = tok $ 
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
                t <- parseType; semicolTok; return $ VarDecl l t})
        ) <?> "Missing or incorrect variable declaration")

parseTypeDecl :: Parser [TypeDecl]
parseTypeDecl = (parseKWtype <?> "expecting keyword 'type'") >> 
    ((many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {l <- parseIdentList; charTok '=';
                t <- parseType; semicolTok; return $ TypeDecl l t})
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
    stmts <- parseStmntList
    return $ Block decls stmts

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
parseExpr = parseSimpleExpr
    >>= \se -> (optionMaybe $
        parseOPrelation >>= \x -> parseSimpleExpr 
                        >>= \y -> return (x, y))
    >>= \m -> return $ case m of Just (x,y) -> Expr se (Just x) (Just y)
                                 Nothing    -> Expr se Nothing Nothing

parseExprList :: Parser ExprList -- non-empty
parseExprList = ExprList <$> many1 parseExpr

parseSimpleExpr :: Parser SimpleExpr
parseSimpleExpr = tok $ (optionMaybe parseOPunary) 
    >>= \m -> parseTerm 
    >>= \t -> (try (many ((,) <$> parseOPadd <*> parseTerm)) <|> (return []))
    >>= \xs -> return $ uncurry (SimpleExpr m t) (unzip xs)

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
    <|> (exactTok "true"  >> return FactorTrue) -- todo double check exactTok is the right choice
    <|> (exactTok "false" >> return FactorFalse) 
    <|> (FactorStr <$> parseString)
    <|> (FactorNum <$> parseNumber)
    <|> (FactorExpr <$> (betweenCharTok '[' ']' parseExpr))
    <|> (FactorDesig <$> parseDesignator)
    <|> (FactorFuncCall <$> parseFuncCall)

parseStmntList :: Parser StatementList -- non-empty
parseStmntList = parseKWbegin 
    >>  (sepBy1 parseStatement semicolTok)
    >>= \stmts -> parseKWend 
    >>= \_     -> return $ StatementList stmts

parseStatement :: Parser Statement 
parseStatement = Statement <$> parseStmntList <|>
    parseAssignment <|> parseIf <|> pure StatementEmpty

parseIf :: Parser Statement
parseIf = do 
    expr  <- between parseKWif parseKWthen parseExpr 
    stmt  <- parseStatement
    mstmt <- optionMaybe $ parseKWelse >> parseStatement
    return $ StatementIf expr stmt mstmt

parseCase :: Parser Statement
parseCase = undefined

parseRepeat :: Parser Statement
parseRepeat = undefined

parseWhile :: Parser Statement
parseWhile = undefined

parseFor :: Parser Statement
parseFor = do
    parseKWfor
    x     <- parseIdent
    stringTok ":="
    expr  <- parseExpr
    direc <- (parseKWto >> pure True) <|> (parseKWdownto >> pure False)
    expr2 <- parseExpr
    parseKWdo
    stmt  <- parseStatement
    return $ StatementFor x expr direc expr2 stmt

parseMem :: Parser Mem
parseMem = undefined

parseAssignment :: Parser Statement
parseAssignment = parseDesignator >>= \x -> stringTok ":="
    >>= \_     -> parseExpr
    >>= \expr  -> return $ Assignment x expr

parseProcCall :: Parser Statement
parseProcCall = undefined

parseStmntMem :: Parser Statement
parseStmntMem = undefined

parseStmntIO :: Parser Statement
parseStmntIO = undefined

parseFuncCall :: Parser FuncCall
parseFuncCall = fail ""

parseNumber :: Parser Number
parseNumber = tok $ do
    pre  <- many1 digit
    post <- ((try $ char '.') >> ('.':) <$> many1 digit) <|> pure ""
    let xs = pre ++ post
    return $ if '.' `elem` xs
            then NUMreal $ read xs
            else NUMint  $ read xs

parseString :: Parser String
parseString = between (char '"') (charTok '"') $ many $
    (noneOf ['\\', '"']) <|>
    ((char '\\') >> anyChar >>= \c -> case toSpecialChar c 
    of Just x  -> return (fromSpecialChar x)
       Nothing -> if c == 'u' then undefined  -- todo hex
                  else unexpected ("char in string " ++ [c]))
