module ProhibitAnnotationsTests where

import Control.Monad.Identity (zipWithM_)
import Language.Java.Rules.ProhibitAnnotations as ProhibitAnnotations (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import qualified Tests

whitelist :: [String]
whitelist = ["FooBar", "Override"]

tests :: Test
tests =
  let file = "/test/java/ProhibitAnnotations.java"
   in test [file ~: Tests.withParsedJavaFile file prohibitAnnotations]

prohibitAnnotations :: CompilationUnit -> FilePath -> Assertion
prohibitAnnotations cUnit path = do
  let expectedRanges =
        [ RDF.Range
            { RDF.start = RDF.Position {RDF.line = 41, RDF.column = Just 5},
              RDF.end = Just (RDF.Position {RDF.line = 42, RDF.column = Just 5})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 43, RDF.column = Just 5},
              RDF.end = Just (RDF.Position {RDF.line = 44, RDF.column = Just 5})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 45, RDF.column = Just 5},
              RDF.end = Just (RDF.Position {RDF.line = 46, RDF.column = Just 5})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 46, RDF.column = Just 5},
              RDF.end = Just (RDF.Position {RDF.line = 47, RDF.column = Just 5})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 47, RDF.column = Just 5},
              RDF.end = Just (RDF.Position {RDF.line = 48, RDF.column = Just 5})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 48, RDF.column = Just 5},
              RDF.end = Just (RDF.Position {RDF.line = 49, RDF.column = Just 5})
            }
        ]
  let diagnostics = ProhibitAnnotations.check whitelist cUnit path
  assertEqual "Check Number of messages" (length expectedRanges) (length diagnostics)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostics)
