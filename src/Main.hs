{-# LANGUAGE OverloadedStrings #-}

module Main where

import Paskell

import Control.Monad.Trans
import System.Console.Haskeline
import Data.List (isPrefixOf)
import qualified Data.Text as T

msg = "Paskell version unknown.\n" ++
    "Type ':l path' to parse a pascal source file."

process :: String -> IO ()
process line = do
    let res = parseToplevel line
    case res of Left err   -> print err
                Right tree -> print tree

processFile :: String -> IO ()
processFile s = do
    res <- parsePascalFile s
    case res of Left err   -> print err
                Right tree -> print tree

repl :: InputT IO ()
repl = do
    minput <- getInputLine "parser> "
    case minput of
        Nothing    -> outputStrLn "Leaving Paskell."
        Just input -> if isPrefixOf ":l " input 
                      then (liftIO $ processFile $ 
                            T.unpack $ T.strip   $ 
                            T.pack $ drop 3 input)  >> repl
                      else (liftIO $ process input) >> repl

main :: IO ()
main = putStrLn msg >> runInputT defaultSettings repl

