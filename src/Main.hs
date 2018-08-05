{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Paskell as P
import qualified ConvertIR as C
import qualified Emit as E
import qualified TypeCheck as T

import Control.Monad.Trans
import System.Console.Haskeline
import Data.List (isPrefixOf)
import qualified Data.Text as Text

import System.IO
import System.Environment

msg = "Paskell version unknown.\n" ++
    "Type ':l path' to compile a pascal source file.\n"
helpmsg = "Usage:\n\tPaskell -c src\n\t" ++ 
    "Paskell -c src dest\n\tPaskell -ir src\n\t" ++ 
    "Paskell -repl\n\tPaskell -h (for help)"

strip s =  Text.unpack $ Text.strip $ Text.pack s

process :: String -> IO ()
process line = do
    let res = P.parseToplevel line
    case res of Left err   -> print err
                Right tree -> print tree

processParse :: String -> IO ()
processParse path = do
    res <- P.parsePascalFile path
    case res of Left err   -> print err
                Right tree -> print tree

processIR :: String -> IO ()
processIR path = do
    e <- P.parsePascalFile path
    case e of Left errP -> print errP 
              Right ast -> case T.typechkProgram ast 
                            of  Left  errTC -> print errTC
                                Right _ -> print $ C.convProgram ast 

processCompile :: String -> String -> IO ()
processCompile path dest = do
    e <- P.parsePascalFile path
    case e of Left errP -> print errP 
              Right ast -> case T.typechkProgram ast of
                                Left  errTC -> print errTC
                                Right _ -> E.printllvm ast >>= 
                                                if dest /= ""
                                                then writeFile dest
                                                else putStrLn

repl :: InputT IO ()
repl = do 
    minput <- getInputLine "Paskell> "
    case minput of
        Nothing    -> outputStrLn "Leaving Paskell."
        Just input  | isPrefixOf ":l " input -> 
                      (liftIO $ processCompile (strip input) "") >> repl
                    | otherwise -> (liftIO $ process (strip input)) >> repl

main :: IO ()
main = do
    args <- getArgs 
    case args of
        [cmd]               | (strip cmd) == "-repl" -> 
                              putStrLn msg >> runInputT defaultSettings repl
                            | otherwise -> putStrLn helpmsg
        [cmd, path]         | (strip cmd) == "-c" ->
                              liftIO (processCompile (strip path) "") 
                            | otherwise -> putStrLn helpmsg
        [cmd, path, dest]   | (strip cmd) == "-c" -> 
                              liftIO $ processCompile (strip path) (strip dest)
                            | otherwise -> putStrLn helpmsg
        _                  -> putStrLn helpmsg
