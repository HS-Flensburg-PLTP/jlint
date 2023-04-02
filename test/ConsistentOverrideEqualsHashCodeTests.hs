module ConsistentOverrideEqualsHashCodeTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.ConsistentOverrideEqualsHashCode (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/ConsistentOverrideEqualsHashCode.java"
   in test [file ~: Tests.withParsedJavaFile file consistentOverride]

consistentOverride :: CompilationUnit -> FilePath -> Assertion
consistentOverride cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 2, column = Just 5},
            end = Just (Position {line = 4, column = Just 5})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 10, column = Just 5},
            end = Just (Position {line = 12, column = Just 5})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 40, column = Just 5},
            end = Just (Position {line = 42, column = Just 5})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 45, column = Just 5},
            end = Just (Position {line = 47, column = Just 5})
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