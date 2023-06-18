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
  [ RDF.mkRange (5, 60) (5, 65),
    RDF.mkRange (10, 60) (10, 65),
    RDF.mkRange (18, 60) (18, 65),
    RDF.mkRange (25, 60) (25, 65)
  ]
