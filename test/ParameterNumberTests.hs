module ParameterNumberTests (tests) where

import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/ParameterNumber.java"
    ParameterNumber.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (5, 5) (6, 6),
    RDF.mkRange (11, 5) (12, 6),
    RDF.mkRange (17, 5) (17, 115)
  ]
