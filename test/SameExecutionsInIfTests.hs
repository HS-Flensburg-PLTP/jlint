module SameExecutionsInIfTests (tests) where

import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "SameExecutionsInIf.java"
    SameExecutionsInIf.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1)
  ]
