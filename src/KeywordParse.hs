module KeywordParse where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.List (find)
import Grammar
import ExtraParsers
import Utils (p')

-- make sure keywords are lower-cased
keywords = [
    -- a
    "and", "array",
    -- b
    "boolean", "begin",
    -- c
    "char", "case", "const",
    -- d
    "do", "downto", "div", "dispose",
    -- e
    "else", "end", 
    -- f
    "for", "function", "file", "forward",
    -- g
    "goto",
    -- h
    -- i
    "if", "integer", "in",
    -- j
    -- k
    -- l
    "label", 
    -- m
    "mod",
    -- n
    "not", "nil", "new",
    -- o
    "or", "of",
    -- p
    "program", "procedure", "packed",
    -- q
    -- r
    "read", "real", "repeat", "record",
    -- s
    "string", "set",
    -- t
    "to", "type", "then",
    -- u
    "until", 
    -- v
    "var", 
    -- w
    "write", "writeln", "with", "while"
    -- x
    -- y
    -- z
    ]

special  = [":=","+","-","*","/","=",
    "<",">","<>","<=",">=","(",")","[",
    "]",",",".",";",":","..","^"]


----------------------------------------------------
-- specialEscape  =  [chr 0x08, chr 0x0C, '\n', '\r',
--     '\t', '\v', '\'', '"',  '\\']
-- isEscapeChar c = c `elem` specialEscape

data SpecialChar = BackSpace | FormFeed
    | NewLine | CarriageReturn | Tab | VerticalTab 
    | SingleQuote | DoubleQuote  | Backslash 
    deriving (Eq, Ord, Show)

fromSpecialChar :: SpecialChar -> Char
fromSpecialChar BackSpace = chr 0x08
fromSpecialChar FormFeed = chr 0x0C
fromSpecialChar NewLine = '\n'
fromSpecialChar CarriageReturn = '\r'
fromSpecialChar Tab = '\t'
fromSpecialChar VerticalTab = '\v'
fromSpecialChar SingleQuote = '\''
fromSpecialChar DoubleQuote = '"'
fromSpecialChar Backslash = '\\'

toSpecialChar :: Char -> Maybe SpecialChar
toSpecialChar c =  snd <$> find ((==) c . fst) table
    where table = [
            ('b',  BackSpace), 
            ('"',  DoubleQuote),
            ('f',  FormFeed),
            ('n',  NewLine),
            ('t',  Tab),
            ('r',  CarriageReturn),
            ('\\', Backslash),
            ('v',  VerticalTab),
            ('\'', SingleQuote)]
    
    ----------------------------------------------------
    
parseReserved :: String -> Reserved -> Parser Reserved
parseReserved kw ctor = exactTok kw >> return ctor

-- a
parseKWand       = parseReserved "and" KWand
parseKWarray     = parseReserved "array" KWarray
-- b
parseKWbegin     = parseReserved "begin" KWbegin
parseKWboolean   = parseReserved "boolean" KWboolean
-- c
parseKWcase      = parseReserved "case" KWcase
parseKWchar      = parseReserved "char" KWchar
parseKWconst     = parseReserved "const" KWconst
-- d
parseKWdiv       = parseReserved "div" KWdiv
parseKWdispose   = parseReserved "dispose" KWdispose
parseKWdownto    = parseReserved "downto" KWdownto
parseKWdo        = parseReserved "do" KWdo
-- e
parseKWelse      = parseReserved "else" KWelse
parseKWend       = parseReserved "end" KWend
-- f
parseKWfile      = parseReserved "file" KWfile
parseKWfor       = parseReserved "for" KWfor
parseKWfunction  = parseReserved "function" KWfunction
parseKWforward  = parseReserved "forward" KWforward
-- g
parseKWgoto      = parseReserved "goto" KWgoto
-- h
-- i
parseKWif        = parseReserved "if" KWif
parseKWin        = parseReserved "in" KWin
parseKWinteger   = parseReserved "integer" KWinteger
-- j
-- k
-- l
parseKWlabel     = parseReserved "label" KWlabel
-- m
parseKWmod       = parseReserved "mod" KWmod
-- n
parseKWnew       = parseReserved "new" KWnew
parseKWnil       = parseReserved "nil" KWnil
parseKWnot       = parseReserved "not" KWnot
-- o
parseKWof        = parseReserved "of" KWof
parseKWor        = parseReserved "or" KWor
-- p
parseKWpacked    = parseReserved "packed" KWpacked
parseKWprocedure = parseReserved "procedure" KWprocedure
parseKWprogram   = parseReserved "program" KWprogram
-- q
-- r
parseKWread      = parseReserved "read" KWread
parseKWreal      = parseReserved "real" KWreal
parseKWrepeat    = parseReserved "repeat" KWrepeat
parseKWrecord    = parseReserved "record" KWrecord
-- s
parseKWset       = parseReserved "set" KWset
parseKWstring    = parseReserved "string" KWstring
-- t
parseKWto        = parseReserved "to" KWto
parseKWtype      = parseReserved "type" KWtype
parseKWthen      = parseReserved "then" KWthen
-- u
parseKWuntil     = parseReserved "until" KWuntil
-- v
parseKWvar       = parseReserved "var" KWvar
-- w
parseKWwhile     = parseReserved "while" KWwhile
parseKWwith      = parseReserved "with" KWwith
parseKWwrite     = parseReserved "write" KWwrite
parseKWwriteln   = parseReserved "writeln" KWwrite
-- x
-- y
-- z