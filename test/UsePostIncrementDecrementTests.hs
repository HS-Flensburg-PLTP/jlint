module UsePostIncrementDecrementTests where

import Control.Monad
import Language.Java.Rules.UsePostIncrementDecrement (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
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
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 3, RDF.column = Just 17},
            RDF.end = Just (RDF.Position {RDF.line = 3, RDF.column = Just 19})
          }
  let expectedRange2 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 5, RDF.column = Just 9},
            RDF.end = Just (RDF.Position {RDF.line = 5, RDF.column = Just 11})
          }
  let expectedRange3 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 6, RDF.column = Just 31},
            RDF.end = Just (RDF.Position {RDF.line = 6, RDF.column = Just 33})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2,
          expectedRange3
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
