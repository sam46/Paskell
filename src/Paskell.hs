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
    (TYident   <$> parseIdent)                 <|>
    (TYboolean <$  stringIgnoreCase "boolean") <|>
    (TYinteger <$  stringIgnoreCase "integer") <|>
    (TYreal    <$  stringIgnoreCase "real")    <|>
    (TYchar    <$  stringIgnoreCase "char")    <|>
    (TYstring  <$  stringIgnoreCase "string")

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
parseProgram = between parseKWprogram (charTok '.') 
    (do prog <- parseIdent
        semicolTok
        blok <- parseBlock
        return $ Program prog blok)

parseBlock :: Parser Block
parseBlock = Block <$> many parseDecl <*> parseStmntList

parseDecl :: Parser Decl
parseDecl = 
    (DeclType <$> parseTypeDecl) <|>
    (DeclVar <$> parseVarDecl) -- <|>
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
        '[' ']' parseExprList)

parseDesigList :: Parser DesigList
parseDesigList = DesigList <$> many1 parseDesignator

parseExprList :: Parser ExprList -- non-empty
parseExprList = ExprList <$> many1 parseExpr

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
    <|> (FactorDesig <$> parseDesignator)
    <|> (FactorFuncCall <$> parseFuncCall)

parseStmntList :: Parser StatementList -- non-empty
parseStmntList = parseKWbegin 
    >>  (sepBy1 parseStatement semicolTok)
    >>= \stmts -> parseKWend 
    >>= \_     -> return $ StatementList stmts

parseStatement :: Parser Statement 
parseStatement = choice [Statement <$> parseStmntList,
    parseAssignment, parseIf, parseFor, pure StatementEmpty]

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

------------------------------------------------------
contents :: Parser a -> Parser a
contents p = do
    whitespace
    r <- p
    eof
    return r

parseToplevel :: String -> Either ParseError Statement
parseToplevel = parse (contents parseStatement) "<stdin>"

parsePascalFile :: String -> IO (Either ParseError Program)
parsePascalFile = parseFromFile (contents parseProgram)
