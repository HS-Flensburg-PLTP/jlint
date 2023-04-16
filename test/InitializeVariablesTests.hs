module InitializeVariablesTests where

import Control.Monad (zipWithM_)
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/InitializeVariables.java"
   in test [file ~: Tests.withParsedJavaFile file initializeVariables]

initializeVariables :: CompilationUnit -> FilePath -> Assertion
initializeVariables cUnit path = do
  let diagnostic = InitializeVariables.check cUnit path
  assertEqual "Check number of messages" 1 (length diagnostic)
  let expectedRange1 =
        Just
          ( Range
              { start = Position {line = 8, column = Just 13},
                end = Just (Position {line = 8, column = Just 64})
              }
          )
  let expectedRanges = [expectedRange1]
  zipWithM_ (assertEqual "Check range") expectedRanges (map (range . location) diagnostic)
