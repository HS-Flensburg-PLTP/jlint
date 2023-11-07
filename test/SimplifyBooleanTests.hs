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
    RDF.mkRange (36, 13) (36, 25),
    RDF.mkRange (44, 13) (44, 26),
    RDF.mkRange (60, 16) (60, 35),
    RDF.mkRange (64, 16) (64, 35)
  ]
