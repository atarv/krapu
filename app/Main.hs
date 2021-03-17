{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Exit
import           System.Environment
import           Parser

parseProgram :: MonadFail m => p -> m a
parseProgram _ = fail "Parsing not implemented"

main :: IO ()
main = do
    getArgs >>= \case
        [] -> do
            putStrLn "No input file given. Exiting..."
            exitFailure
        [sourcePath] -> parseProgram sourcePath
        _            -> do
            putStrLn "Only one input file allowed"
            exitFailure

