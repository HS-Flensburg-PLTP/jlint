module ReduceScopeTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.ReduceScope as ReduceScope (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/ReduceScope.java"
   in test [file ~: Tests.withParsedJavaFile file reduceScope]

reduceScope :: CompilationUnit -> FilePath -> Assertion
reduceScope cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 29, column = Just 9},
            end = Just (Position {line = 36, column = Just 10})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 41, column = Just 9},
            end = Just (Position {line = 48, column = Just 10})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 56, column = Just 10},
            end = Just (Position {line = 75, column = Just 11})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 56, column = Just 10},
            end = Just (Position {line = 75, column = Just 11})
          }
  let expectedRange5 =
        Range
          { start = Position {line = 56, column = Just 10},
            end = Just (Position {line = 75, column = Just 11})
          }

  let expectedRange6 =
        Range
          { start = Position {line = 87, column = Just 9},
            end = Just (Position {line = 98, column = Just 10})
          }
  let expectedRange7 =
        Range
          { start = Position {line = 115, column = Just 9},
            end = Just (Position {line = 125, column = Just 10})
          }
  let expectedRange8 =
        Range
          { start = Position {line = 130, column = Just 9},
            end = Just (Position {line = 141, column = Just 10})
          }
  let expectedRange9 =
        Range
          { start = Position {line = 172, column = Just 9},
            end = Just (Position {line = 178, column = Just 10})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2,
          expectedRange3,
          expectedRange4,
          expectedRange5,
          expectedRange6,
          expectedRange7,
          expectedRange8,
          expectedRange9
        ]
  let diagnostic = ReduceScope.check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
