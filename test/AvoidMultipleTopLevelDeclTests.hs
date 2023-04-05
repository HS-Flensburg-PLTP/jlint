module AvoidMultipleTopLevelDeclTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.AvoidMultipleTopLevelDecl (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import Tests

tests :: Test
tests =
  let file = "/test/java/AvoidMultipleTopLevelDecl.java"
   in test [file ~: Tests.withParsedJavaFile file avoidMultipleTopLevelDecl]

avoidMultipleTopLevelDecl :: CompilationUnit -> FilePath -> Assertion
avoidMultipleTopLevelDecl cUnit path = do
  let expectedRange1 =
        RDF.Range
          { RDF.start = RDF.Position {RDF.line = 5, RDF.column = Just 1},
            RDF.end = Just (RDF.Position {RDF.line = 7, RDF.column = Just 1})
          }
  let expectedRanges =
        [ expectedRange1
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
