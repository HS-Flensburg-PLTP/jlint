module UsePostIncrementDecrementTests where

import Tests (withParsedJavaFile)
import RDF 
import Test.HUnit
import Language.Java.Syntax ( CompilationUnit )
import Control.Monad 
import Language.Java.Rules.UsePostIncrementDecrement (check)

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