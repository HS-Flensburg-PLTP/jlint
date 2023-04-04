module UseJavaArrayTypeStyleTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/UseJavaArrayTypeStyle.java"
   in test [file ~: Tests.withParsedJavaFile file useJavaArrayTypeStyle]

useJavaArrayTypeStyle :: CompilationUnit -> FilePath -> Assertion
useJavaArrayTypeStyle cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 3, column = Just 12},
            end = Just (Position {line = 3, column = Just 20})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 6, column = Just 13},
            end = Just (Position {line = 6, column = Just 22})
          }
  let expectedRange3 =
        Range
          { start = Position {line = 10, column = Just 23},
            end = Just (Position {line = 10, column = Just 30})
          }
  let expectedRange4 =
        Range
          { start = Position {line = 11, column = Just 13},
            end = Just (Position {line = 11, column = Just 24})
          }
  let expectedRange5 =
        Range
          { start = Position {line = 14, column = Just 39},
            end = Just (Position {line = 14, column = Just 45})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2,
          expectedRange3,
          expectedRange4,
          expectedRange5
        ]
  let diagnostic = UseJavaArrayTypeStyle.check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
