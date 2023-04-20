module AvoidMultipleTopLevelDeclTests (tests) where

import qualified Language.Java.Rules.AvoidMultipleTopLevelDecl as AvoidMultipleTopLevelDecl
import qualified RDF
import System.FilePath ((</>))
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
  rangesTest
    []
    (javaPathTo "SingleTopLevelDecl.java")
    AvoidMultipleTopLevelDecl.check

multipleDeclsTest :: Test
multipleDeclsTest =
  rangesTest
    expectedRanges
    (javaPathTo "MultipleTopLevelDecl.java")
    AvoidMultipleTopLevelDecl.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (5, 1) (7, 2)]

javaPathTo :: FilePath -> FilePath
javaPathTo = ("AvoidMultipleTopLevelDecl" </>)
