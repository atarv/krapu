{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Text                      ( Text )
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Analyzer
import           Interpreter
import           Parser

import qualified Data.Text.IO                  as T

data Flag
    = ParseOnly
    | Interpret
    | Repl
    | Help
    | Check

options :: [OptDescr Flag]
options =
    [ Option ['r'] ["repl"] (NoArg Repl) "Evaluate statements interactively"
    , Option ['h'] ["help"] (NoArg Help) "Print this help information"
    , Option ['i']
             ["interpret"]
             (NoArg Interpret)
             "Parse and interpret a program (default)"
    , Option ['p'] ["parse"] (NoArg ParseOnly) "Parse the program and print AST"
    , Option ['c'] ["check"] (NoArg Check)     "Typecheck file"
    ]

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        ([Help], _, []) -> putStrLn $ usageInfo "Krapu interpreter" options
        ([ParseOnly], []        , []  ) -> parseAst "stdin" stdin
        ([ParseOnly], [src]     , []  ) -> withFile src ReadMode (parseAst src)
        ([Interpret], []        , []  ) -> run "stdin" [] stdin
        ([Interpret], src : args, []  ) -> withFile src ReadMode (run src args)
        ([Check    ], []        , []  ) -> run "stdin" [] stdin
        ([Check    ], src : _   , []  ) -> withFile src ReadMode (checkFile src)
        ([Repl     ], args      , []  ) -> repl args
        ([]         , []        , []  ) -> run "stdin" [] stdin
        ([]         , src : args, []  ) -> withFile src ReadMode (run src args)
        (flags      , srcs      , errs) -> do
            let plural = if length errs > 1 then "s" else ""
            putStrLn $ "Error" <> plural <> " parsing command line arguments:"
            when (length flags > 1)
                $  putStrLn
                $  "Too many flags given. Expected 0 or 1, got "
                <> show (length flags)
            when (length srcs > 1)
                $  putStrLn
                $  "Too many source files given. Expected 0 or 1, got "
                <> show (length srcs)
            mapM_ putStr errs
            exitFailure

parseAst :: String -> Handle -> IO ()
parseAst src handle = do
    content <- T.hGetContents handle
    either (const exitFailure . T.putStrLn) T.putStrLn
        =<< pure (parseProgram src content)

run :: String -> [String] -> Handle -> IO ()
run src args handle = do
    content <- T.hGetContents handle
    let crate = parseCrate src content >>= analyzeCrate
    case crate of
        Left  err   -> T.putStrLn err >> exitFailure
        Right crate -> runProgram args $ optimizeCrate crate

prompt :: Text
prompt = "krapu>"

repl :: [String] -> IO ()
repl args = runRepl args prompInput
  where
    prompInput = do
        T.hPutStr stdout prompt
        hFlush stdout
        input <- T.getLine
        case parseStatement "repl" input of
            Left err -> do
                T.putStrLn err
                prompInput
            Right stmt -> pure stmt

checkFile :: String -> Handle -> IO ()
checkFile src handle = do
    content <- T.hGetContents handle
    case parseCrate src content >>= analyzeCrate of
        Left err -> do
            T.putStrLn err
            exitFailure
        Right _ -> do
            putStrLn $ "Checked " <> src
            exitSuccess
