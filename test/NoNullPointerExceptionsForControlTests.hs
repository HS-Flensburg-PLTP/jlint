module NoNullPointerExceptionsForControlTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl (check)
import Language.Java.Syntax
import qualified RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/NoNullPointerExceptionsForControl.java"
   in test [file ~: Tests.withParsedJavaFile file nullPointerExceptions]

nullPointerExceptions :: CompilationUnit -> FilePath -> Assertion
nullPointerExceptions cUnit path = do
  let expectedRange1 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 7, RDF.column = Just 13},
            RDF.end = Just (RDF.Position {RDF.line = 12, RDF.column = Just 9})
          }
  let expectedRange2 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 19, RDF.column = Just 13},
            RDF.end = Just (RDF.Position {RDF.line = 24, RDF.column = Just 9})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2
        ]
  let diagnostic = NoNullPointerExceptionsForControl.check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
