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
  []
