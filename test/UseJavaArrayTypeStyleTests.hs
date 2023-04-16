module UseJavaArrayTypeStyleTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle (check)
import Language.Java.Syntax
import qualified RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/UseJavaArrayTypeStyle.java"
   in test [file ~: Tests.withParsedJavaFile file useJavaArrayTypeStyle]

useJavaArrayTypeStyle :: CompilationUnit -> FilePath -> Assertion
useJavaArrayTypeStyle cUnit path = do
  let expectedRange1 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 3, RDF.column = Just 12},
            RDF.end = Just (RDF.Position {RDF.line = 3, RDF.column = Just 20})
          }
  let expectedRange2 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 6, RDF.column = Just 13},
            RDF.end = Just (RDF.Position {RDF.line = 6, RDF.column = Just 21})
          }
  let expectedRange3 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 8, RDF.column = Just 13},
            RDF.end = Just (RDF.Position {RDF.line = 8, RDF.column = Just 23})
          }
  let expectedRange4 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 11, RDF.column = Just 23},
            RDF.end = Just (RDF.Position {RDF.line = 11, RDF.column = Just 30})
          }
  let expectedRange5 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 13, RDF.column = Just 39},
            RDF.end = Just (RDF.Position {RDF.line = 13, RDF.column = Just 45})
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
    (map (RDF.range . RDF.location) diagnostic)
