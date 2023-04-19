module AvoidMultipleTopLevelDeclTests (tests) where

import qualified Language.Java.Rules.AvoidMultipleTopLevelDecl as AvoidMultipleTopLevelDecl
import qualified RDF
import Test.HUnit (Test, test, (~:))
import Tests

tests :: Test
tests =
  test
    [ "SingleDeclaration" ~: singleDeclTest,
      "MultipleDeclarations" ~: multipleDeclsTest
    ]

singleDeclTest :: Test
singleDeclTest =
  goodFileTest
    "/test/java/AvoidMultipleTopLevelDecl/SingleTopLevelDecl.java"
    AvoidMultipleTopLevelDecl.check

multipleDeclsTest :: Test
multipleDeclsTest =
  rangesTest
    expectedRanges
    "/test/java/AvoidMultipleTopLevelDecl/MultipleTopLevelDecl.java"
    AvoidMultipleTopLevelDecl.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (5, 1) (7, 2)]
