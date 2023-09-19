module MultipleVariableDeclarationsTests (tests) where

import qualified Language.Java.Rules.MultipleVariableDeclarations as MultipleVariableDeclarations
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "MultipleVariableDeclarations.java"
    MultipleVariableDeclarations.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (2, 5) (2, 22),
    RDF.mkRange (7, 9) (7, 18),
    RDF.mkRange (9, 9) (10, 15),
    RDF.mkRange (12, 9) (12, 30)
  ]
