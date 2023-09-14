module SingleTopLevelClassTests (tests) where

import qualified Language.Java.Rules.SingleTopLevelClass as SingleTopLevelClass
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
    ("SingleTopLevelClass" </> "SingleTopLevelClass.java")
    SingleTopLevelClass.check

multipleDeclsTest :: Test
multipleDeclsTest =
  rangesTest
    expectedRanges
    ("SingleTopLevelClass" </> "MultipleTopLevelClasses.java")
    SingleTopLevelClass.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (5, 1) (7, 2)]
