{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Exception
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           AST
import           Interpreter
import           Parser

import qualified Data.Text.IO                  as T

data Flag
    = ParseOnly
    | Interpret
    | Repl
    | Help

options :: [OptDescr Flag]
options =
    [ Option ['r'] ["repl"] (NoArg Repl) "Evaluate statements interactively"
    , Option ['h'] ["help"] (NoArg Help) "Print this help information"
    , Option ['i']
             ["interpret"]
             (NoArg Interpret)
             "Parse and interpret a program (default)"
    , Option ['p'] ["parse"] (NoArg ParseOnly) "Parse the program and print AST"
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
    either printParseError (runProgram args) $ parseCrate src content
    where printParseError err = T.putStrLn err >> exitFailure

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
