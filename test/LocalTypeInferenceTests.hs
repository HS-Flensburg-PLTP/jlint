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
  [ RDF.mkRange (4, 13) (4, 18),
    RDF.mkRange (9, 22) (9, 48),
    RDF.mkRange (11, 21) (11, 33),
    RDF.mkRange (12, 22) (12, 42),
    RDF.mkRange (14, 15) (14, 31)
  ]
