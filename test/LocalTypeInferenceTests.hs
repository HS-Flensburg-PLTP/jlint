module LocalTypeInferenceTests where

import qualified Language.Java.Rules.LocalTypeInference as LocalTypeInference
import qualified RDF
import Test.HUnit
import Tests (rangesTest)

tests :: Test
tests =
  rangesTest
    expectedRanges
    "LocalTypeInference.java"
    LocalTypeInference.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 9) (4, 19),
    RDF.mkRange (9, 9) (9, 49),
    RDF.mkRange (11, 21) (11, 33),
    RDF.mkRange (12, 9) (12, 43),
    RDF.mkRange (14, 9) (14, 32)
  ]
