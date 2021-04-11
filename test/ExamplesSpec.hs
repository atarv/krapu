module ExamplesSpec where

import           Control.Monad
import           Data.Either
import           System.Directory
import           System.IO
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           AST
import           Interpreter
import           Parser

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

testDir :: String
testDir = "test/examples/"

-- | Check that parsing the program succeeds
parseExample :: String -> SpecWith ()
parseExample program =
    it program
        $ (   T.readFile (testDir <> program)
          >>= \source -> pure $ isRight (parseProgram program source)
          )
        `shouldReturn` True

-- | Check that example program won't crash. Doesn't warrant in any way that the
-- program returns correct result.
runExample :: String -> SpecWith ()
runExample program = it program $ do
    let run = do
            source <- T.readFile (testDir <> program)
            case parseCrate program source of
                Left  err   -> fail $ T.unpack err
                Right crate -> runProgram crate
    run `shouldReturn` ()

spec :: Spec
spec = do
    examplePrograms <- runIO $ listDirectory testDir
    describe "Parsing example programs" $ do
        forM_ examplePrograms parseExample
    describe "Running example programs" $ do
        forM_ examplePrograms runExample
