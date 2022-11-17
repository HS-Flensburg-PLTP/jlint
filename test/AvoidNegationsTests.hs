module AvoidNegationsTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.AvoidNegations (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import Tests

tests :: Test
tests =
  let file = "/test/java/AvoidNegations.java"
   in test [file ~: Tests.withParsedJavaFile file avoidNegations]

avoidNegations :: CompilationUnit -> FilePath -> Assertion
avoidNegations cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 6, column = Just 9},
            end = Just (Position {line = 10, column = Just 9})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 15, column = Just 9},
            end = Just (Position {line = 19, column = Just 5})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 22, column = Just 9},
            end = Just (Position {line = 28, column = Just 9})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 42, column = Just 9},
            end = Just (Position {line = 46, column = Just 9})
          }
  let expectedRange5 =
        Range
          { start = Position {line = 50, column = Just 9},
            end = Just (Position {line = 54, column = Just 9})
          }
  let expectedRange6 =
        Range
          { start = Position {line = 60, column = Just 13},
            end = Just (Position {line = 64, column = Just 13})
          }
  let expectedRange7 =
        Range
          { start = Position {line = 72, column = Just 24},
            end = Just (Position {line = 72, column = Just 81})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2,
          expectedRange3,
          expectedRange4,
          expectedRange5,
          expectedRange6,
          expectedRange7
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
