module RedundantModifiersTests where

import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/RedundantModifiers.java"
    RedundantModifiers.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (3, 5) (3, 11),
    RDF.mkRange (4, 5) (4, 13),
    RDF.mkRange (5, 5) (5, 11),
    RDF.mkRange (5, 12) (5, 20)
  ]
