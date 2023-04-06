module NoStarImportsTests where

import Control.Monad
import Language.Java.Rules.NoStarImports (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  let file = "/test/java/NoStarImports.java"
   in test [file ~: Tests.withParsedJavaFile file noStarImports]

noStarImports :: CompilationUnit -> FilePath -> Assertion
noStarImports cUnit path = do
  let expectedRange1 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
            RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
          }
  let expectedRange2 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
            RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
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
    (map (RDF.range . RDF.location) diagnostic)
