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
            end = Just (Position {line = 5, column = Just 9})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 13, column = Just 9},
            end = Just (Position {line = 14, column = Just 9})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRange5 =
        Range
          { start = Position {line = 22, column = Just 9},
            end = Just (Position {line = 23, column = Just 9})
          }
  let expectedRange6 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRange7 =
        Range
          { start = Position {line = 31, column = Just 9},
            end = Just (Position {line = 32, column = Just 9})
          }
  let expectedRange8 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRange9 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRange10 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRanges =
        [expectedRange1, expectedRange2, expectedRange3, expectedRange4, expectedRange5, expectedRange6, expectedRange7, expectedRange8, expectedRange9, expectedRange10]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
