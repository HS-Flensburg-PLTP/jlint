module AvoidMultipleVarDeclTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.AvoidMultipleVarDecl (check)
import Language.Java.Syntax
import qualified RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import Tests

tests :: Test
tests =
  let file = "/test/java/AvoidMultipleVarDecl.java"
   in test [file ~: Tests.withParsedJavaFile file avoidMultipleVarDecl]

avoidMultipleVarDecl :: CompilationUnit -> FilePath -> Assertion
avoidMultipleVarDecl cUnit path = do
  let expectedRange1 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 6, RDF.column = Just 9},
            RDF.end = Just (RDF.Position {RDF.line = 8, RDF.column = Just 9})
          }
  let expectedRange2 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 8, RDF.column = Just 9},
            RDF.end = Just (RDF.Position {RDF.line = 11, RDF.column = Just 9})
          }
  let expectedRange3 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 11, RDF.column = Just 9},
            RDF.end = Just (RDF.Position {RDF.line = 12, RDF.column = Just 5})
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
