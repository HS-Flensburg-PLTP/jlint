module ConstantPropagationTests where

import qualified Language.Java.Rules.ConstantPropagation as ConstantPropagation
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "ConstantPropagation.java"
    ConstantPropagation.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 9) (7, 10),
    RDF.mkRange (9, 9) (12, 10),
    RDF.mkRange (14, 9) (19, 10),
    RDF.mkRange (21, 9) (26, 10)
  ]
