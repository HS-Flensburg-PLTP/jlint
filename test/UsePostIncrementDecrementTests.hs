module UsePostIncrementDecrementTests (tests) where

import qualified Language.Java.Rules.UsePostIncrementDecrement as UsePostIncrementDecrement
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "UsePostIncrementDecrement.java"
    UsePostIncrementDecrement.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (3, 17) (3, 20),
    RDF.mkRange (5, 9) (5, 12),
    RDF.mkRange (6, 31) (6, 34)
  ]
