{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Exit
import           System.Environment
import           System.Console.GetOpt
import           System.IO
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Parser
import           Interpreter

data Flag
    = ParseOnly
    | Interpret
    | Repl
    | Help

options :: [OptDescr Flag]
options =
    [ Option ['e'] ["repl"] (NoArg Repl) "Evaluate statements interactively"
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
        ([ParseOnly], []   , []  ) -> parse "stdin" stdin
        ([ParseOnly], [src], []  ) -> openFile src ReadMode >>= parse src
        ([Interpret], []   , []  ) -> fail "not implemented"
        ([Interpret], [src], []  ) -> fail "not implemented"
        ([Repl     ], []   , []  ) -> fail "not implemented"
        ([Repl     ], [src], []  ) -> fail "not implemented"
        ([]         , []   , []  ) -> fail "not implemented"
        ([]         , [src], []  ) -> fail "not implemented"
        (_          , _    , errs) -> do
            let plural = if length errs > 1 then "s" else ""
            putStrLn $ "Error" <> plural <> " parsing command line arguments:"
            mapM_ putStr errs
            exitFailure 

parse :: String -> Handle -> IO ()
parse src handle = do
    content <- T.hGetContents handle
    case parseProgram src content of
        Left err -> do
            T.putStrLn err
            exitFailure
        Right ast -> do
            T.putStrLn ast
