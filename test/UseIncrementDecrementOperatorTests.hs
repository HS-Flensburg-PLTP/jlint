module UseIncrementDecrementOperatorTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.UseIncrementDecrementOperator (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import Tests (withParsedJavaFile)

tests :: Test
tests =
  let file = "/test/java/UseIncrementDecrementOperator.java"
   in test [file ~: Tests.withParsedJavaFile file useIncrementDecrementOperator]

useIncrementDecrementOperator :: CompilationUnit -> FilePath -> Assertion
useIncrementDecrementOperator cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 4, column = Just 9},
            end = Just (Position {line = 5, column = Just 5})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 8, column = Just 9},
            end = Just (Position {line = 9, column = Just 5})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 12, column = Just 9},
            end = Just (Position {line = 9, column = Just 5})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 16, column = Just 9},
            end = Just (Position {line = 9, column = Just 5})
          }
  let expectedRanges =
        [expectedRange1, expectedRange2, expectedRange3, expectedRange4]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)