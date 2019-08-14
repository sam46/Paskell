{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Paskell as P
import qualified ConvertIR as C
import qualified Emit as E
import qualified TypeCheck as T

import Control.Monad.Trans
-- import System.Console.Haskeline
import Data.List (isPrefixOf)
import qualified Data.Text as Text

import System.IO
import System.Environment
import System.Process

-- msg = "Paskell version unknown.\n" 
--     ++ "Type ':l path' to compile a pascal source file.\n"
helpmsg = "Usage:\n  "
    ++ "Paskell -c src      compile to llvm-ir\n  "
    ++ "Paskell -c src dest compile to llvm-ir and save in dest\n  "
    ++ "Paskell -ir src     produce IR\n  "
    ++ "Paskell -x src      execute pascal source. Equivalent to Paskell -c src | lli\n  " 
    -- ++ "Paskell -repl\n\t" 
    ++ "Paskell -h          (for help)"

strip s =  Text.unpack $ Text.strip $ Text.pack s

-- process :: String -> IO ()
-- process line = do
--     let res = P.parseToplevel line
--     case res of Left err   -> print err
--                 Right tree -> print tree

-- processParse :: String -> IO ()
-- processParse path = do
--     res <- P.parsePascalFile path
--     case res of Left err   -> print err
--                 Right tree -> print tree

-- repl :: InputT IO ()
-- repl = do 
--     minput <- getInputLine "Paskell> "
--     case minput of
--         Nothing    -> outputStrLn "Leaving Paskell."
--         Just input  | isPrefixOf ":l " input -> 
--                       (liftIO $ processCompile (strip input) "") >> repl
--                     | otherwise -> (liftIO $ process (strip input)) >> repl

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
                                Right _ -> E.printllvm ast (E.toShortBS path) >>= 
                                                if dest /= ""
                                                then writeFile dest
                                                else putStrLn       -- stdout

processExec :: String -> IO ()
processExec path = do
    e <- P.parsePascalFile path
    case e of Left errP -> print errP 
              Right ast -> case T.typechkProgram ast of
                                Right _     -> E.printllvm ast (E.toShortBS path) >>= execLLVM
                                Left errTC  -> print errTC
      
execLLVM :: String -> IO ()
execLLVM llvm = do
    (excode,res,stderr) <- readCreateProcessWithExitCode (shell "lli") llvm
    putStr res
    if length stderr > 0 
        then putStr $ stderr ++ "\n" ++ (show excode)  
        else putStr ""

main :: IO ()
main = do
    args <- getArgs 
    case args of
        -- [cmd]               | (strip cmd) == "-repl" -> 
        --                       putStrLn msg >> runInputT defaultSettings repl
        --                     | otherwise -> putStrLn helpmsg
        [cmd, path]         | (strip cmd) == "-c" ->
                              liftIO (processCompile (strip path) "") 
                            | (strip cmd) == "-x" ->
                              liftIO (processExec (strip path)) 
                            | (strip cmd) == "-ir" ->
                              liftIO (processIR (strip path)) 
                            | otherwise -> putStrLn helpmsg
        [cmd, path, dest]   | (strip cmd) == "-c" -> 
                              liftIO $ processCompile (strip path) (strip dest)
                            | otherwise -> putStrLn helpmsg
        _                  -> putStrLn helpmsg
