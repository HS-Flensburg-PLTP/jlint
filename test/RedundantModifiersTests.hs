module RedundantModifiersTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.RedundantModifiers (check)
import Language.Java.Syntax
import qualified RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/RedundantModifiers.java"
   in test [file ~: Tests.withParsedJavaFile file redundantModifiers]

redundantModifiers :: CompilationUnit -> FilePath -> Assertion
redundantModifiers cUnit path = do
  let expectedRange1 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 3, RDF.column = Just 5},
            RDF.end = Just (RDF.Position {RDF.line = 3, RDF.column = Just 11})
          }
  let expectedRange2 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 4, RDF.column = Just 5},
            RDF.end = Just (RDF.Position {RDF.line = 4, RDF.column = Just 13})
          }
  let expectedRange3 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 5, RDF.column = Just 5},
            RDF.end = Just (RDF.Position {RDF.line = 5, RDF.column = Just 11})
          }
  let expectedRange4 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 5, RDF.column = Just 12},
            RDF.end = Just (RDF.Position {RDF.line = 5, RDF.column = Just 20})
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
    (map (RDF.range . RDF.location) diagnostic)
