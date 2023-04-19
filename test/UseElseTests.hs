module UseElseTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.UseElse (check)
import Language.Java.Syntax
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import Tests

tests :: Test
tests =
  let file = "/test/java/UseElse.java"
   in test [file ~: Tests.withParsedJavaFile file useElse]

useElse :: CompilationUnit -> FilePath -> Assertion
useElse cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 4, column = Just 9},
            end = Just (Position {line = 6, column = Just 10})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 11, column = Just 10},
            end = Just (Position {line = 13, column = Just 11})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 14, column = Just 10},
            end = Just (Position {line = 21, column = Just 11})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 30, column = Just 9},
            end = Just (Position {line = 32, column = Just 10})
          }
  let expectedRange5 =
        Range
          { start = Position {line = 41, column = Just 9},
            end = Just (Position {line = 46, column = Just 10})
          }
  let expectedRange6 =
        Range
          { start = Position {line = 52, column = Just 9},
            end = Just (Position {line = 58, column = Just 10})
          }
  let expectedRange7 =
        Range
          { start = Position {line = 83, column = Just 9},
            end = Just (Position {line = 96, column = Just 10})
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
