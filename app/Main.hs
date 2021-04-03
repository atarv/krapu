{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           System.Exit
import           System.Environment
import           System.Console.GetOpt
import           System.IO
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
        ([ParseOnly], []   , []  ) -> parseAst "stdin" stdin
        ([ParseOnly], [src], []  ) -> withFile src ReadMode (parseAst src)
        ([Interpret], []   , []  ) -> run "stdin" stdin
        ([Interpret], [src], []  ) -> withFile src ReadMode (run src)
        ([Repl     ], []   , []  ) -> fail "not implemented"
        ([Repl     ], [src], []  ) -> fail "not implemented"
        ([]         , []   , []  ) -> run "stdin" stdin
        ([]         , [src], []  ) -> withFile src ReadMode (run src)
        (flags      , srcs , errs) -> do
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

run :: String -> Handle -> IO ()
run src handle = do
    content <- T.hGetContents handle
    either printParseError runProgram =<< pure (parseCrate src content)
    where printParseError err = T.putStrLn err >> exitFailure
