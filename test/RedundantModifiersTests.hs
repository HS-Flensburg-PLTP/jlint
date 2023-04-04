module RedundantModifiersTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.RedundantModifiers (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/RedundantModifiers.java"
   in test [file ~: Tests.withParsedJavaFile file redundantModifiers]

redundantModifiers :: CompilationUnit -> FilePath -> Assertion
redundantModifiers cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 3, column = Just 5},
            end = Just (Position {line = 3, column = Just 12})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 4, column = Just 5},
            end = Just (Position {line = 4, column = Just 14})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 5, column = Just 5},
            end = Just (Position {line = 5, column = Just 12})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 5, column = Just 12},
            end = Just (Position {line = 5, column = Just 21})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2,
          expectedRange3,
          expectedRange4
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
