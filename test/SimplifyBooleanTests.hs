module SimplifyBooleanTests (tests) where

import qualified Language.Java.Rules.SimplifyBoolean as SimplifyBoolean
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "SimplifyBoolean.java"
    SimplifyBoolean.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (8, 9) (12, 10),
    RDF.mkRange (16, 9) (20, 10),
    RDF.mkRange (28, 13) (28, 25),
    RDF.mkRange (36, 13) (36, 26),
    RDF.mkRange (52, 16) (52, 35),
    RDF.mkRange (56, 16) (56, 35)
  ]
