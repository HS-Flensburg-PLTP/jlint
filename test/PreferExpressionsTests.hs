module PreferExpressionsTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.PreferExpressions as PreferExpressions (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/PreferExpressions.java"
   in test [file ~: Tests.withParsedJavaFile file preferExpressions]

preferExpressions :: CompilationUnit -> FilePath -> Assertion
preferExpressions cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 7, column = Just 13},
            end = Just (Position {line = 9, column = Just 13})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 8, column = Just 13},
            end = Just (Position {line = 10, column = Just 13})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 9, column = Just 13},
            end = Just (Position {line = 11, column = Just 13})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 10, column = Just 13},
            end = Just (Position {line = 11, column = Just 13})
          }
  let expectedRange5 =
        Range
          { start = Position {line = 19, column = Just 13},
            end = Just (Position {line = 21, column = Just 13})
          }
  let expectedRange6 =
        Range
          { start = Position {line = 20, column = Just 13},
            end = Just (Position {line = 22, column = Just 13})
          }
  let expectedRange7 =
        Range
          { start = Position {line = 21, column = Just 13},
            end = Just (Position {line = 23, column = Just 13})
          }
  let expectedRange8 =
        Range
          { start = Position {line = 22, column = Just 13},
            end = Just (Position {line = 23, column = Just 13})
          }
  let expectedRange9 =
        Range
          { start = Position {line = 32, column = Just 13},
            end = Just (Position {line = 34, column = Just 13})
          }
  let expectedRange10 =
        Range
          { start = Position {line = 33, column = Just 13},
            end = Just (Position {line = 35, column = Just 13})
          }
  let expectedRange11 =
        Range
          { start = Position {line = 34, column = Just 13},
            end = Just (Position {line = 35, column = Just 13})
          }
  let expectedRange12 =
        Range
          { start = Position {line = 43, column = Just 13},
            end = Just (Position {line = 45, column = Just 13})
          }
  let expectedRange13 =
        Range
          { start = Position {line = 44, column = Just 13},
            end = Just (Position {line = 46, column = Just 13})
          }
  let expectedRange14 =
        Range
          { start = Position {line = 45, column = Just 13},
            end = Just (Position {line = 46, column = Just 13})
          }
  let expectedRange15 =
        Range
          { start = Position {line = 55, column = Just 13},
            end = Just (Position {line = 57, column = Just 9})
          }
  let expectedRange16 =
        Range
          { start = Position {line = 63, column = Just 10},
            end = Just (Position {line = 65, column = Just 10})
          }
  -- This should be the result of this test
  --         Range
  --           { start = Position {line = 63, column = Just 10},
  --             end = Just (Position {line = 64, column = Just 54})
  --           }
  let expectedRange17 =
        Range
          { start = Position {line = 78, column = Just 13},
            end = Just (Position {line = 79, column = Just 13})
          }
  let expectedRange18 =
        Range
          { start = Position {line = 102, column = Just 13},
            end = Just (Position {line = 103, column = Just 13})
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
          expectedRange9,
          expectedRange10,
          expectedRange11,
          expectedRange12,
          expectedRange13,
          expectedRange14,
          expectedRange15,
          expectedRange16,
          expectedRange17,
          expectedRange18
        ]
  let diagnostic = PreferExpressions.check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
