module ProhibitAnnotationsTests where

import Control.Monad.Identity (zipWithM_)
import Language.Java.Rules.ProhibitAnnotations as ProhibitAnnotations (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/ProhibitAnnotations.java"
   in test [file ~: Tests.withParsedJavaFile file prohibitAnnotations]

prohibitAnnotations :: CompilationUnit -> FilePath -> Assertion
prohibitAnnotations cUnit path = do
  let expectedRanges =
        [ RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            }
        ]
  let diagnostics = ProhibitAnnotations.check cUnit path
  assertEqual "Check Number of messages" (length expectedRanges) (length diagnostics)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostics)
