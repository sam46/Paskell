module ExtraParsers where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Utils (p')

-- todo: multiline comments
comments :: Parser () 
comments = try comment <|> return ()
  where
    nestedComment = comments >> comments
    comment = do
      _ <- stringTok "(*"
      skipMany $ noneOf "(**)"
      try nestedComment
      _ <- stringTok "*)"
      return ()

whitespace :: Parser ()
whitespace = spaces >> comments

-- Case insensitive
charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = char (toUpper c) <|> char (toLower c)

stringIgnoreCase :: String -> Parser String
stringIgnoreCase []     = return []
stringIgnoreCase (x:xs) = (:) <$> charIgnoreCase x <*> stringIgnoreCase xs

tok :: Parser a -> Parser a
tok = (<* whitespace)

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

betweenStringTok :: String -> String -> Parser a -> Parser a
betweenStringTok s1 s2 p = between (stringTok s1) (stringTok s2) p

betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma c1 c2 p = betweenCharTok c1 c2 $
  sepBy p (charTok ',')

exactTok :: String -> Parser String -- case is ignored!
exactTok s = tok . try $ 
  do { stringIgnoreCase s; notFollowedBy alphaNum; return s }
