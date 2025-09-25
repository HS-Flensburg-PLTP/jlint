module UseLocalTypeInferenceTests (tests) where

import qualified Language.Java.Rules.UseLocalTypeInference as UseLocalTypeInference
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "UseLocalTypeInference.java"
    UseLocalTypeInference.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (13, 9) (13, 25),
    RDF.mkRange (14, 9) (14, 35),
    RDF.mkRange (15, 9) (15, 56),
    RDF.mkRange (18, 9) (18, 49),
    RDF.mkRange (40, 9) (40, 39)
  ]
