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
  let diagnostic = UseJavaArrayTypeStyle.check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
