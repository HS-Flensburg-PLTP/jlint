module ParameterNumberTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.ParameterNumber (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import Tests

tests :: Test
tests =
  let file = "/test/java/ParameterNumber.java"
   in test [file ~: Tests.withParsedJavaFile file parameterNumber]

parameterNumber :: CompilationUnit -> FilePath -> Assertion
parameterNumber cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 5, column = Just 5},
            end = Just (Position {line = 6, column = Just 5})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 11, column = Just 5},
            end = Just (Position {line = 12, column = Just 5})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 17, column = Just 5},
            end = Just (Position {line = 18, column = Just 5})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2,
          expectedRange3
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
