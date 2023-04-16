module ConsistentOverrideEqualsHashCodeTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.ConsistentOverrideEqualsHashCode (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/ConsistentOverrideEqualsHashCode.java"
   in test [file ~: Tests.withParsedJavaFile file consistentOverride]

consistentOverride :: CompilationUnit -> FilePath -> Assertion
consistentOverride cUnit path = do
  let expectedRange1 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 2, RDF.column = Just 5},
            RDF.end = Just (RDF.Position {RDF.line = 4, RDF.column = Just 6})
          }
  let expectedRange2 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 10, RDF.column = Just 5},
            RDF.end = Just (RDF.Position {RDF.line = 12, RDF.column = Just 6})
          }
  let expectedRange3 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 40, RDF.column = Just 5},
            RDF.end = Just (RDF.Position {RDF.line = 42, RDF.column = Just 6})
          }
  let expectedRange4 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 45, RDF.column = Just 5},
            RDF.end = Just (RDF.Position {RDF.line = 47, RDF.column = Just 6})
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
