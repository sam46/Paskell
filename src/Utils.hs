module Utils where

import Text.Parsec
import Text.Parsec.String

p' :: Parser a -> String -> Either ParseError a
p' p = parse p ""

enumerate = zip [0..]