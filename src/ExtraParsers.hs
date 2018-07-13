module ExtraParsers where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Utils (p')

comments :: Parser ()
comments = spaces -- todo change to actual comment parser

whitespace :: Parser ()
-- whitespace = do {spaces; many comments} -- todo un-comment once comments parser is done 
whitespace = spaces >> comments 

charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = char (toUpper c) <|> char (toLower c)

stringIgnoreCase :: String -> Parser String
stringIgnoreCase [] = return []
stringIgnoreCase (x:xs) = do {c <- charIgnoreCase x; cs <- stringIgnoreCase xs; return (c:cs)}

tok :: Parser a -> Parser a
tok p = p >>= \x -> whitespace >>= \_ -> return x 

charTok :: Char -> Parser Char
charTok = tok . char  

charIgnoreCaseTok :: Char -> Parser Char
charIgnoreCaseTok c = charTok (toUpper c) <|> charTok (toLower c)

commaTok :: Parser Char
commaTok = charTok ','

semicolTok :: Parser Char
semicolTok = charTok ';'

-- charsIgnoreCaseTok :: String -> Parser String
-- charsIgnoreCaseTok = tok . charsIgnoreCase 

stringTok :: String -> Parser String
stringTok = tok . string -- Doesn't parse exact words! e.g. parse (stringTok "ab") "abc" correctly consumes "ab"

betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok c1 c2 p = between (charTok c1) (charTok c2) p

betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma c1 c2 p = betweenCharTok c1 c2 $
  sepBy p (charTok ',')

exactTok :: String -> Parser String -- case is ignored!
exactTok s = tok . try $ 
  do {stringIgnoreCase s; notFollowedBy alphaNum; return s}