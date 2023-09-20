module FinalParametersTests (tests) where

import qualified Language.Java.Rules.FinalParameters as FinalParameters
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "FinalParameters.java"
    FinalParameters.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (9, 28) (9, 39),
    RDF.mkRange (12, 40) (12, 47),
    RDF.mkRange (15, 21) (15, 32),
    RDF.mkRange (18, 21) (18, 33),
    RDF.mkRange (18, 35) (18, 47),
    RDF.mkRange (48, 18) (48, 30)
  ]
