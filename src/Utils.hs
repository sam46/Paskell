module Utils (parse') where

import Text.Parsec
import Text.Parsec.String

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""
