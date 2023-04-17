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
            end = Just (Position {line = 4, column = Just 18})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 5, column = Just 9},
            end = Just (Position {line = 5, column = Just 18})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 6, column = Just 9},
            end = Just (Position {line = 6, column = Just 18})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 7, column = Just 9},
            end = Just (Position {line = 7, column = Just 15})
          }
  let expectedRange5 =
        Range
          { start = Position {line = 9, column = Just 35},
            end = Just (Position {line = 9, column = Just 44})
          }
  let expectedRange6 =
        Range
          { start = Position {line = 12, column = Just 35},
            end = Just (Position {line = 12, column = Just 41})
          }
  let expectedRange7 =
        Range
          { start = Position {line = 15, column = Just 16},
            end = Just (Position {line = 15, column = Just 25})
          }
  let expectedRanges =
        [expectedRange1, expectedRange2, expectedRange3, expectedRange4, expectedRange5, expectedRange6, expectedRange7]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
