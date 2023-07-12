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
  [ RDF.mkRange (4, 9) (9, 10),
    RDF.mkRange (15, 16) (19, 10),
    RDF.mkRange (23, 9) (29, 10),
    RDF.mkRange (31, 9) (41, 10),
    RDF.mkRange (43, 9) (53, 10)
  ]
