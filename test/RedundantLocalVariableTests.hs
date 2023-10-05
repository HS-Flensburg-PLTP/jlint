module RedundantLocalVariableTests (tests) where

import qualified Language.Java.Rules.RedundantLocalVariable as RedundantLocalVariable
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "RedundantLocalVariable.java"
    RedundantLocalVariable.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (7, 13) (7, 29),
    RDF.mkRange (9, 13) (9, 21),
    RDF.mkRange (14, 13) (14, 18)
  ]
