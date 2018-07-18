module Tests where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Utils
import Paskell
import ExtraParsers
import KeywordParse
import Grammar
import TypeCheck

-- import Control.Exception (ErrorCall(ErrorCall), evaluate)
-- import Test.HUnit.Base  ((~?=), Test(TestCase, TestList))
-- import Test.HUnit.Text (runTestTT)
-- import Test.HUnit.Tools (assertRaises)

checkPass :: Parser a -> String -> Bool
checkPass p inp = case p' p inp of Right _ -> True
                                   Left  _ -> False
checkFail p inp = not $ checkPass p inp

test :: [(Parser a, Bool, String)] -> IO ()
test testcases = do
    let res = map (\(p, doPass, inp) -> (if doPass then checkPass else checkFail) p inp) testcases
    putStrLn $ "Running " ++ (show $ length res) ++ " tests:"
    mapM putStrLn $ (`map` (enumerate res))
        (\(i, x) -> 
            (show $ i+1) ++ (if x then "... OK" else "... FAILED"))
    putStrLn $ "Result: " ++ (show $ length $ filter id res) ++ "/" ++ (show $ length res) ++ " tests passed!"
    return ()

-- testParser parseIdent True ["X", "111"]

testParser p doPass xs = let tuple3 (a,b) c = (a,b,c) 
    in test $ zipWith tuple3 (repeat (p, doPass)) xs 

tparseIdent = do
    testParser parseIdent True [
        "X1",
        "x1",
        "x1  ",
        "Xyz1 ",
        "X1z  ",
        "xyz",
        "xYz  ",
        "X1@ ", -- should only consume X1
        "var123 " ]
    testParser parseIdent False [
        "",
        "1123",
        "1X1 ",
        " xyz",
        "Var" ]

tparseKeywords = do
    testParser parseKWand True [
        "and",
        "ANd",
        "and ",
        "ANd  \n " ]
    testParser parseKWand False [
        "and123",
        "sand" ]

tparseIdentList = do
    testParser parseIdentList True [
        "x,y",
        "x, y ",
        "x1 , y2 , Z ",
        "var1 , y2 , Z ",
        "x1 , y2 , Z" ]
    testParser parseIdentList False [
        "",
        "x1, var1, var",
        "x1, @, x2" ]
    
tparseVarDecl = do
    testParser parseVarDecl True [
        "var x,y:char;",
        "var x,y:char; ",
        "var x,y:char1; ",
        "var x,y:char;abc",
        "var x , y : char ; abc",
        "var x,y:char; var1 , var2, var3: char ; abc",
        "var x,y:char; var" ]
    testParser parseVarDecl False [
        "var",
        "x,y:char;",
        "var x,y:char@;",
        "var x,var:char;",
        "var :char; x2:char;" ]

tparseTypeDecl = do
    testParser parseTypeDecl True [
        "type x,y=char;",
        "type x,y=char1;",
        "type x,y=char; ",
        "type x,y=char;abc",
        "type x , y = char ; abc",
        "type x,y=char; var1 , var2, var3= char ; abc",
        "type x,y=char; type",
        "type x,y=char; z=boolean; type w=char;abc " ]
    testParser parseTypeDecl False [
        "type",
        "x,y=char;",
        "x,y=char;",
        "type x,y=char@;",
        "type x,type=char;",
        "type =char; x2=char;" ]

tparseBlock = do
    testParser parseBlock True [
        "",
        "@abc ",
        "var x,y:char; z:boolean; var w:char;abc ",
        "var x,y:char; z:boolean; var w:char; abc",
        "var x,y:char; z:boolean; var w:char; type x,y=char; var1 , var2, var3= char ; abc",
        "var x , y : char ; z:boolean; var w:char; type x , y = char ; var1 , var2, var3= char ; abc ",
        "type x,y=char; z=boolean; type w=char;abc ",
        "type x,y=char; z=boolean; type w=char;" ++ "  type x,y=char; z=boolean; type w=char;" 
            ++ "var x2,y2:char; z2:boolean; var w2:char; abc" ]
    testParser parseBlock False [
        "type",
        "var",
        "var x=char;",
        "var x:char; type y:char;",
        "var x:char; type var=char;" ]

tparseProgram = do
    testParser parseProgram True [    
        "program mypro; var x:char;.", 
        "program mypro; var x:char1;.", 
        "  program   mypro  ;   var x : char;. ", 
        "program mypro;.", 
        "program mypro; var x:char; type t=boolean; ." ]
    testParser parseProgram False [    
        "program mypro; var x:char;", 
        "program mypro; var x:char@;.",  
        " mypro; var x:char;.", 
        "  program   mypro   var x : char;. ", 
        "program var;.", 
        "program mypro; var x:char; var." ]

tparseOP = do
    testParser parseOP True $ (map fst operators) ++ [
        "+123",
        "== ", -- should work! only consumes "="
        "divxyz ", -- works too!
        "<> " ]
    testParser parseOP False ["", "@", ":", " +"]

tparseDesigProp = do
    testParser parseDesigProp True [
        ".x",
        ". x abcd ",
        ".var123"    ]

tparseDesignator = undefined

tparseTerm = do
    testParser parseTerm True [
        "nil ",
        "true",
        "x",
        "true and x",
        "\"hello\"",
        "\"hello\" * \"123\"",
        "true and false ",
        "true mod \"hello\"",
        -- follwing cases will succeed by only consuming some of the input
        "true abcd",
        "true + false",
        "\"hello\" \"123\"",
        "true false",
        "true *"]
    testParser parseTerm False [
        "",
        "* true"]

-- tparseString = do 
--     testParser parseString True [
        -- ]

--     mapM (\s -> putStrLn $ s ++ " :\n" ++ show (p' parseString s)) [
--         "\"\"",
--         "\" \" ",
--         "\"abc\" ",
--         "\" ab \" ",
--         "\" \\\\abc\" ",
--         "\" \ \nabc\" ",
--         "\" \\xyz\" ",
--         "\" \\\\ \" ",
--         "\"\\ab\\\\c\" " ]
    
--     -- testParser parseString False [
--     --     "\" \\abc\" ",
--     -- ]

tparseSimpleExpr = undefined

tparseExpr = do
    testParser (parseExpr<*eof) True [
        "1  ",
        "x",
        "(1)",    
        "1+2",
        "1+2+3+4",
        "x+y+z",
        "1*2",
        "1*2*3*4",
        "-1+2",
        "-1+2+3",
        "1+2*3+4",
        "1*2+3*4",
        "1+2*3*4",
        "1*2*3+4",
        "1*2*x+4",
        "x*2*3+4",
        "(1+2)",
        "(1+x)",
        "(-1+2)",
        "(-1+2+3)",
        "(1+2*3+4)",
        "(1*2+3*4)",
        "(1+2*3*4)",
        "(1*2*3+4)",
        "(1*2*x+4)",
        "(x*2*3+4)",
        "-(1+2)",
        "-(1+2)+3",
        "-1",
        "-1*2",
        "1*(-2)",
        "1*(-2+3)",
        "1*(-2*3)",
        "1+(2+3)*4"]
    testParser (parseExpr<*eof) False [
        "1*-2"]


tparseStmntList = undefined

tparseIf = undefined

tparseStatement = undefined

tparseNumber = do
    testParser parseNumber True [
        "123",
        "0123",
        "12.3",
        "123   ",
        "0123abc",
        "12.3 ",
        "12.3abc"]
    testParser parseNumber False [
        "123.",
        ".123",
        "a123"]

tparseFor = do
    testParser parseFor True [
        "for x:= 2 downto 10 do y := 3 ",
        "for x:= 2 downto 10 do y :=3",
        "for x:= w downto 10.5 do y :=3",
        "for x123:= 3+3 to 5-5 do begin y:=3;z:=10+5end"]
    testParser parseFor False [
        "",
        "x:= 2 downto 10 do y := 3 ",
        "for x : 2 downto 10 do y :=3",
        "for123 x:= 3+3 to 5-5 do begin y:=3;z:=10+5end",
        "for x:= 3+3 down 5-5 do y:=true"]


tgettype = 
    let foo s env t = case p' parseStatement s 
            of Right st -> putStrLn $ show $ t == typechk env st  in
    do
        foo "x := 1" [(Ident "x", TYint)] [(Ident "x", TYint)] 
        foo "x := 1+1" [(Ident "x", TYint)] [(Ident "x", TYint)]
        foo "x := 1+1*2" [(Ident "x", TYint)] [(Ident "x", TYint)] 
        foo "x := 1.2" [(Ident "x", TYreal)] [(Ident "x", TYreal)] 
        foo "x := 1.2 + 1.2" [(Ident "x", TYreal)] [(Ident "x", TYreal)] 
        foo "x := 1 + 1.2" [(Ident "x", TYreal)] [(Ident "x", TYreal)] 
        foo "x := 1.2 + 1" [(Ident "x", TYreal)] [(Ident "x", TYreal)] 
        foo "x := True or False" [(Ident "x", TYbool)] [(Ident "x", TYbool)] 
        foo "x := True or y" [(Ident "x", TYbool), (Ident "y", TYbool)] [(Ident "x", TYbool), (Ident "y", TYbool)]
        foo "x := +1" [(Ident "x", TYint)] [(Ident "x", TYint)]  
        foo "x := + 1.2" [(Ident "x", TYreal)] [(Ident "x", TYreal)]  
        foo "x := (1 < 2)" [(Ident "x", TYbool)] [(Ident "x", TYbool)]
        foo "x := (1 < 2.3)" [(Ident "x", TYbool)] [(Ident "x", TYbool)]
        foo "x := (true > false)" [(Ident "x", TYbool)] [(Ident "x", TYbool)]
        foo "x := (\"a\" = \"b\")" [(Ident "x", TYbool)] [(Ident "x", TYbool)]
        foo "x := true or (1 < 2)" [(Ident "x", TYbool)] [(Ident "x", TYbool)]
        foo "x := (1 < y) or z" [(Ident "x", TYbool), (Ident "z", TYbool), (Ident "y", TYreal)] [(Ident "x", TYbool), (Ident "z", TYbool), (Ident "y", TYreal)]
        
        -- Fail:
        -- foo "x := True or y" [(Ident "x", TYbool), (Ident "y", TYint)] [(Ident "x", TYbool), (Ident "y", TYint)] 
   
ttypechkIf = 
    let foo s env t = case p' parseStatement s 
            of Right st -> putStrLn $ show $ t == typechk env st in
    do
        foo "if true then x:=1" [(Ident "x", TYint)] [(Ident "x", TYint)] 
        foo "if true then x:=1 else x:= false" [(Ident "x", TYint)] [(Ident "x", TYint)] 
        foo "if true then x:=1 else x:= false" [(Ident "x", TYint)] [(Ident "x", TYint)] 


testAll = do
    tparseKeywords
    tparseIdent
    tparseIdentList
    tparseVarDecl
    tparseTypeDecl
    tparseBlock
    tparseProgram
    tparseOP
    tparseTerm
    tparseNumber
    tparseFor
    tparseExpr
    -- tparseDesigProp
    -- tparseDesignator
    -- tparseString

testFiles = do
    let path = "f:/Paskell/pascal-src/"
    parseFromFile parseProgram $ path ++ "p1.pas"
