module KeywordParse where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char

import Grammar
import ExtraParsers
import Utils (p')

parseReserved :: String -> Reserved -> Parser Reserved
parseReserved kw ctor = tok . try $ 
    do { stringIgnoreCase kw; notFollowedBy alphaNum; return ctor }

parseKWand       = parseReserved "and" KWand
parseKWdownto    = parseReserved "downto" KWdownto
parseKWif        = parseReserved "if" KWif
parseKWor        = parseReserved "or" KWor
parseKWthen      = parseReserved "then" KWthen
parseKWarray     = parseReserved "array" KWarray
parseKWelse      = parseReserved "else" KWelse
parseKWin        = parseReserved "in" KWin
parseKWpacked    = parseReserved "packed" KWpacked
parseKWto        = parseReserved "to" KWto
parseKWbegin     = parseReserved "begin" KWbegin
parseKWend       = parseReserved "end" KWend
parseKWlabel     = parseReserved "label" KWlabel
parseKWprocedure = parseReserved "procedure" KWprocedure
parseKWtype      = parseReserved "type" KWtype
parseKWcase      = parseReserved "case" KWcase
parseKWfile      = parseReserved "file" KWfile
parseKWmod       = parseReserved "mod" KWmod
parseKWprogram   = parseReserved "program" KWprogram
parseKWuntil     = parseReserved "until" KWuntil
parseKWconst     = parseReserved "const" KWconst
parseKWfor       = parseReserved "for" KWfor
parseKWnil       = parseReserved "nil" KWnil
parseKWrecord    = parseReserved "record" KWrecord
parseKWvar       = parseReserved "var" KWvar
parseKWdiv       = parseReserved "div" KWdiv
parseKWfunction  = parseReserved "function" KWfunction
parseKWnot       = parseReserved "not" KWnot
parseKWrepeat    = parseReserved "repeat" KWrepeat
parseKWwhile     = parseReserved "while" KWwhile
parseKWdo        = parseReserved "do" KWdo
parseKWgoto      = parseReserved "goto" KWgoto
parseKWof        = parseReserved "of" KWof
parseKWset       = parseReserved "set" KWset
parseKWwith      = parseReserved "with" KWwith