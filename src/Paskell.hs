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
    else return ident

parseType :: Parser Type
parseType = tok $ 
    (TYident <$> parseIdent)                 <|>
    (TYbool  <$  stringIgnoreCase "boolean") <|>
    (TYint   <$  stringIgnoreCase "integer") <|>
    (TYreal  <$  stringIgnoreCase "real")    <|>
    (TYchar  <$  stringIgnoreCase "char")    <|>
    (TYstr   <$  stringIgnoreCase "string")

parseIdentList :: Parser IdentList
parseIdentList = sepBy1 parseIdent commaTok

parseDeclVar :: Parser Decl -- var a,b : char; c,d : integer;
parseDeclVar = DeclVar <$>( (parseKWvar <?> "expecting keyword 'var'") >>
    ((concat <$> (many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {xs <- parseIdentList; charTok ':';
             t <- parseType; semicolTok; 
             return $ zip xs (repeat t)})
     )) <?> "Missing or incorrect variable declaration"))

parseDeclType :: Parser Decl
parseDeclType = DeclType <$> ((parseKWtype <?> "expecting keyword 'type'") >> 
    ((concat <$> (many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {xs <- parseIdentList; charTok '=';
             t <- parseType; semicolTok; 
             return $ zip xs (repeat t)})
     )) <?> "Missing or incorrect type declaration"))

parseConstDecl :: Parser [ConstDecl]
parseConstDecl = undefined -- todo

parseProgram :: Parser Program
parseProgram = between parseKWprogram (charTok '.') 
    (do prog <- parseIdent
        semicolTok
        blok <- parseBlock
        return $ Program prog blok)

parseBlock :: Parser Block
parseBlock = Block <$> many parseDecl <*> parseStmntSeq

parseDecl :: Parser Decl
parseDecl = 
    (parseDeclType) <|>
    (parseDeclVar)  <|>
    (parseDeclFunc) <|>
    (parseDeclProc) -- <|>
    -- (DeclConst <$> parseDeclConst)

makeOPparser :: [(String, OP)] -> Parser OP
makeOPparser xs = let f (a, b) = try (stringTok a >> return b) 
    in foldr (<|>) (fail "Expecting operator") (map f xs)
parseOP         = makeOPparser operators -- any OP (relation, additive, mult, unary)
parseOPunary    = {-OPunary    <$>-} makeOPparser unaryops
parseOPadd      = {-OPadd      <$>-} makeOPparser addops
parseOPmult     = {-OPmult     <$>-} makeOPparser multops
parseOPrelation = {-OPrelation <$>-} makeOPparser relationops

parseDesignator :: Parser Designator
parseDesignator = Designator <$> parseIdent <*> try (many parseDesigProp)

parseDesigProp :: Parser DesigProp
parseDesigProp =
    (charTok '.' >> DesigPropIdent <$> parseIdent) <|>
    (charTok '^' >> return DesigPropPtr)           <|>
    (DesigPropExprList <$> betweenCharTok 
        '[' ']' parseExprList1)

parseDesigList :: Parser DesigList
parseDesigList = DesigList <$> many1 parseDesignator

parseExprList1 :: Parser ExprList -- non-empty
parseExprList1 = sepBy1 parseExpr commaTok

parseExprList :: Parser ExprList -- non-empty
parseExprList = sepBy parseExpr commaTok

parseExpr :: Parser Expr
parseExpr = (try $ Relation <$> 
        parseSimpleExpr <*> parseOPrelation <*> parseSimpleExpr)
    <|> parseSimpleExpr

parseSimpleExpr :: Parser Expr
parseSimpleExpr = (try simpleAdd)
    <|> (try $ Unary <$> parseOPunary <*> simpleAdd)
    <|> (try $ Unary <$> parseOPunary <*> parseSimpleExpr)
    <|> parseTerm
    where simpleAdd = Add <$> parseTerm <*> parseOPadd <*> parseSimpleExpr

parseTerm :: Parser Expr
parseTerm = (try $ Mult <$> 
        parseFactor <*> parseOPmult <*> parseTerm)
    <|> parseFactor

parseFactor :: Parser Expr
parseFactor = 
        (parseKWnil >> return FactorNil)
    <|> (parseKWnot >> FactorNot <$> parseFactor)
    <|> (exactTok "true"  >> return FactorTrue) -- todo double check exactTok is the right choice
    <|> (exactTok "false" >> return FactorFalse) 
    <|> (parseNumber)
    <|> (FactorStr <$> parseString)
    <|> (betweenCharTok '(' ')' parseExpr)
    <|> (try parseFuncCall)
    <|> (FactorDesig <$> parseDesignator)

parseStmntSeq :: Parser Statement -- non-empty
parseStmntSeq = parseKWbegin 
    >>  (sepBy1 parseStatement semicolTok)
    >>= \stmts -> parseKWend 
    >>= \_     -> return $ StatementSeq stmts

parseStatement :: Parser Statement 
parseStatement = choice [parseStmntSeq,
    parseAssignment, parseIf, parseFor,
    parseWhile, pure StatementEmpty]

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
parseWhile = do
    parseKWwhile
    ex <- parseExpr
    parseKWdo
    s <- parseStatement
    return $ StatementWhile ex s

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

parseStmtNew :: Parser Statement
parseStmtNew = undefined

parseStmtDispose :: Parser Statement
parseStmtDispose = undefined

parseAssignment :: Parser Statement
parseAssignment = parseDesignator >>= \x -> stringTok ":="
    >>= \_     -> parseExpr
    >>= \expr  -> return $ Assignment x expr

parseProcCall :: Parser Statement
parseProcCall = ProcCall <$> parseIdent 
    <*> ((betweenCharTok '(' ')' parseExprList) 
         <|> pure [])

parseStmntMem :: Parser Statement
parseStmntMem = undefined

parseStmntWrite :: Parser Statement
parseStmntWrite = StatementWrite <$> (parseKWwrite >>
     ((betweenCharTok '(' ')' parseExprList)))

parseStmntWriteLn :: Parser Statement
parseStmntWriteLn = StatementWriteLn <$> (parseKWwriteln >>
    ((betweenCharTok '(' ')' parseExprList)))

parseFuncCall :: Parser Expr
parseFuncCall = FuncCall <$> parseIdent 
    <*> ((betweenCharTok '(' ')' parseExprList))

parseNumber :: Parser Expr
parseNumber = tok $ do
    pre  <- many1 digit
    post <- ((try $ char '.') >> ('.':) <$> many1 digit) <|> pure ""
    let xs = pre ++ post
    return $ if '.' `elem` xs
            then FactorReal $ read xs
            else FactorInt  $ read xs

parseString :: Parser String
parseString = between (char '"') (charTok '"') $ many $
    (noneOf ['\\', '"']) <|>
    ((char '\\') >> anyChar >>= \c -> case toSpecialChar c 
    of Just x  -> return (fromSpecialChar x)
       Nothing -> if c == 'u' then undefined  -- todo hex
                  else unexpected ("char in string " ++ [c]))

-- parseSubprogDeclList
parseDeclProc :: Parser Decl 
parseDeclProc = do
    parseKWprocedure
    f <- parseIdent
    params <- (try parseFormalParams) <|> (pure [])
    semicolTok
    blk <- parseBlock
    semicolTok
    return $ DeclProc f params blk

parseDeclFunc :: Parser Decl
parseDeclFunc = do
    parseKWfunction
    f <- parseIdent
    params <- (try parseFormalParams) <|> (pure [])
    charTok ':'
    rtype <- parseType
    semicolTok
    blk <- parseBlock
    semicolTok
    return $ DeclFunc f params rtype blk

parseFormalParams :: Parser [(Ident,Type,Bool)]
parseFormalParams = (concatMap id) <$>
    (betweenCharTok '(' ')' $ sepBy parseFormalParam semicolTok)

parseFormalParam :: Parser [(Ident,Type,Bool)]
parseFormalParam = do
    mvar <- (== Nothing) <$> (optionMaybe parseKWvar)
    idents <- parseIdentList
    charTok ':'
    t <- parseType
    return $ map (\(x, ty) -> (x, ty, mvar)) (zip idents $ repeat t)

------------------------------------------------------
contents :: Parser a -> Parser a
contents p = whitespace *> p <* eof

parseToplevel :: String -> Either ParseError Program
parseToplevel = parse (contents parseProgram) "<stdin>"

parsePascalFile :: String -> IO (Either ParseError Program)
parsePascalFile = parseFromFile (contents parseProgram)
