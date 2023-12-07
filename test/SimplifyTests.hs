module SimplifyTests (tests) where

import qualified Language.Java.Rules.Simplify as Simplify
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "Simplify.java"
    Simplify.check

expectedRanges :: [RDF.Range]
expectedRanges = []

-- [ RDF.mkRange (4, 13) (4, 48),
-- RDF.mkRange (5, 20) (5, 55)
-- ]
