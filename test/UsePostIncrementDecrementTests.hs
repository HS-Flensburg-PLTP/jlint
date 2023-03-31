module UsePostIncrementDecrementTests where

import Control.Monad
import Language.Java.Rules.UsePostIncrementDecrement (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
  ( Diagnostic (location),
    Location (range),
    Position (Position, column, line),
    Range (Range, end, start),
  )
import Test.HUnit
import Tests (withParsedJavaFile)

tests :: Test
tests =
  let file = "/test/java/UsePostIncrementDecrement.java"
   in test [file ~: Tests.withParsedJavaFile file usePostIncrementDecrement]

usePostIncrementDecrement :: CompilationUnit -> FilePath -> Assertion
usePostIncrementDecrement cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 1, column = Just 1},
            end = Just (Position {line = 1, column = Just 1})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
