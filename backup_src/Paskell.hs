module Paskell where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Grammar
import ExtraParsers
import KeywordParse
import Utils (p')

-- | Parse Identifier
parseIdent :: Parser Ident
parseIdent = tok . try $ do
    x  <- letter
    xs <- many alphaNum
    let ident = x:xs
    if (toLower <$> ident) `elem` keywords 
    then fail ("parseIdent: Expecting identifier but found keyword \'" ++ ident ++ "\'")
    else return ident

-- | Parse array declaration
parseArrayDecl :: Parser Type
parseArrayDecl = tok . try $ do
    exactTok "array"
    mbsz <- optionMaybe $ betweenCharTok '[' ']' (many1 digit)
    exactTok "of"
    ty <- parseType
    return $ TYarr (read <$> mbsz) ty

-- | Parse Type
parseType :: Parser Type
parseType = tok $ 
    (TYident <$> parseIdent)                  <|>
    (TYbool  <$  stringIgnoreCase "boolean")  <|>
    (TYint   <$  stringIgnoreCase "integer")  <|>
    (TYreal  <$  stringIgnoreCase "real")     <|>
    (TYchar  <$  stringIgnoreCase "char")     <|>
    (TYstr   <$  stringIgnoreCase "string")   <|>
    parseArrayDecl


-- | Parse Identifier List
parseIdentList :: Parser IdentList
parseIdentList = sepBy1 parseIdent commaTok

-- | Parse Variable Declaration
-- | var a,b : char;
parseDeclVar :: Parser Decl
parseDeclVar = DeclVar <$> ( (parseKWvar <?> "expecting keyword 'var'") >>
    ((concat <$> (many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {xs <- parseIdentList; charTok ':';
             t <- parseType; semicolTok; 
             return $ zip xs (repeat t)})
     )) <?> "Missing or incorrect variable declaration"))

-- | Parse Type Declaration
parseDeclType :: Parser Decl
parseDeclType = DeclType <$> ((parseKWtype <?> "expecting keyword 'type'") >> 
    ((concat <$> (many1 $ try  -- todo try separating many1 into initial parse and then many for better error messages
        (do {xs <- parseIdentList; charTok '=';
             t <- parseType; semicolTok; 
             return $ zip xs (repeat t)})
     )) <?> "Missing or incorrect type declaration"))

parseConstDecl :: Parser [ConstDecl]
parseConstDecl 
    = error $ "parseConstDecl" -- todo

-- | Parse Program definition
parseProgram :: Parser Program
parseProgram = between parseKWprogram (charTok '.')     -- between 'program' and '.'
    (do prog <- parseIdent  -- program name
        semicolTok          -- semicolon ;
        block <- parseBlock -- statement block
        return $ Program prog block)

-- | Parse Block (Declarations ++ Statements)
parseBlock :: Parser Block
parseBlock = Block <$> many parseDecl <*> parseStmtSeq

-- | Parse Declaration
parseDecl :: Parser Decl
parseDecl
    =   (parseDeclType)     -- type declaration
    <|> (parseDeclVar)      -- variable declaration
    <|> (parseDeclFunc)     -- function declaration
    <|> (parseDeclProc)     -- procedure declaration
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

-- | Parse Factors
parseFactor :: Parser Expr
parseFactor = 
        (parseKWnil >> return FactorNil)
    <|> (parseKWnot >> FactorNot <$> parseFactor)
    <|> (exactTok "true"  >> return FactorTrue) -- todo double check exactTok is the right choice
    <|> (exactTok "false" >> return FactorFalse) 
    <|> (parseNumber)
    <|> (try $ FactorChar <$> parseChar)
    <|> (FactorStr <$> parseString)
    <|> (betweenCharTok '(' ')' parseExpr)
    <|> (try parseFuncCall)
    <|> (FactorDesig <$> parseDesignator)

-- | Parse Block of Statements (begin stmt_block end)
parseStmtSeq :: Parser Statement -- non-empty
parseStmtSeq = parseKWbegin 
    >>  (sepBy1 parseStatement semicolTok)
    >>= \stmts -> parseKWend 
    >>= \_     -> return $ StatementSeq stmts

-- | Parse a statement
parseStatement :: Parser Statement 
parseStatement = choice [parseStmtSeq,         -- statements block
    (try parseAssignment),                      -- assignment
    (try parseProcCall),                        -- procedure call
    parseIf, parseFor,                          -- if, for
    -- parseCase, parseStmtNew, parseStmtDispose,  -- case, new, dispose
    parseWhile, parseStmtWriteLn,              -- while, writeln
    parseStmtWrite,                            -- write
    pure StatementEmpty]                        -- emptyStmtBlock

-- | Parse If expr then stmt1 (else stmt2)?
parseIf :: Parser Statement
parseIf = do 
    expr  <- between parseKWif parseKWthen parseExpr 
    stmt  <- parseStatement
    mstmt <- optionMaybe $ parseKWelse >> parseStatement
    return $ StatementIf expr stmt mstmt

-- | Parse Case of
parseCase :: Parser Statement
parseCase = error $ "parseCase"

-- | Parse Repeat loop
parseRepeat :: Parser Statement
parseRepeat = error $ "parseRepeat"

-- | Parse While loop
parseWhile :: Parser Statement
parseWhile = do
    parseKWwhile
    ex <- parseExpr
    parseKWdo
    s <- parseStatement
    return $ StatementWhile ex s

-- | Parse For loop
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

-- | Parse New
-- | "new" [ "[" <expr> "]" ] <l-value> 
parseStmtNew :: Parser Statement
parseStmtNew = do
    parseKWnew
    -- mbarraySize <- (Just $ try parseSimpleExpr) <|> (pure Nothing)
    mbarraySizeExpr <- optionMaybe $ (try parseSimpleExpr)
    ident <- parseIdent
    return $ StatementNew ident mbarraySizeExpr

-- | Parse Dispose
-- | "dispose" [ "[" "]" ] <l-value>
parseStmtDispose :: Parser Statement
parseStmtDispose = do
    parseKWdispose
    let isArray = True  -- todo: implement isArray.
    ident <- parseIdent
    return $ StatementDispose ident isArray

-- | Parse Assignment
-- | <l-value> ":=" <expr>
parseAssignment :: Parser Statement
parseAssignment = parseDesignator >>= \x -> stringTok ":="
    >>= \_     -> parseExpr
    >>= \expr  -> return $ Assignment x expr

-- | Parse a procedure call
parseProcCall :: Parser Statement
parseProcCall = ProcCall <$> parseIdent 
    <*> ((betweenCharTok '(' ')' parseExprList) 
         <|> pure [])

parseStmtMem :: Parser Statement
parseStmtMem = error $ "parseStmtMem"

parseStmtWrite :: Parser Statement
parseStmtWrite = StatementWrite <$> (parseKWwrite >>
     ((betweenCharTok '(' ')' parseExprList)))

parseStmtWriteLn :: Parser Statement
parseStmtWriteLn = StatementWriteLn <$> (parseKWwriteln >>
    ((betweenCharTok '(' ')' parseExprList)))

-- | Parse a function call
parseFuncCall :: Parser Expr
parseFuncCall = FuncCall <$> parseIdent 
    <*> ((betweenCharTok '(' ')' parseExprList))

-- | Parse a numeric value integer or double
parseNumber :: Parser Expr
parseNumber = tok $ do
    pre  <- many1 digit
    post <- ((try $ char '.') >> ('.':) <$> many1 digit) <|> pure ""
    let xs = pre ++ post
    return $ if '.' `elem` xs
            then FactorReal $ read xs
            else FactorInt  $ read xs

-- | Parse a string between double quotes
parseString :: Parser String
parseString = between (char '\'') (charTok '\'') $ many $
    (noneOf ['\\', '\'']) <|>
    ((char '\\') >> anyChar >>= \c -> case toSpecialChar c 
    of Just x  -> return (fromSpecialChar x)
       Nothing -> if c == 'u' then error $ "parseString: Undefined"  -- todo hex
                  else unexpected ("char in string" ++ [c]))

-- | Parse a character between single quotes
parseChar :: Parser Char
parseChar = between (char '\'') (charTok '\'') $
    (noneOf ['\\', '\'']) <|>
    ((char '\\') >> anyChar >>= \c -> case toSpecialChar c 
    of Just x  -> return (fromSpecialChar x)
       Nothing -> if c == 'u' then error $ "parseChar: Undefined"  -- todo hex
                  else unexpected ("char literal" ++ [c]))

-- | Parse a procedure declaration
parseDeclProc :: Parser Decl 
parseDeclProc = do
    parseKWprocedure
    f <- parseIdent
    params <- (try parseFormalParams) <|> (pure [])
    semicolTok
    blk <- parseBlock
    semicolTok
    return $ DeclProc f params blk

-- | Parse a function declaration
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

-- | Parse formal parameters [(name, type, byReference)]
parseFormalParams :: Parser [(Ident,Type,Bool)]
parseFormalParams = (concatMap id) <$>
    (betweenCharTok '(' ')' $ sepBy parseFormalParam semicolTok)

-- var x,y,z : int => [(x, int, True), (y, int, True), (z, int, True)]
-- x,y,z : int => [(x, int, False), (y, int, False), (z, int, False)]
parseFormalParam :: Parser [(Ident,Type,Bool)]
parseFormalParam = do
    mvar <- (/= Nothing) <$> (optionMaybe parseKWvar)
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
