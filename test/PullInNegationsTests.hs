module PullInNegationsTests (tests) where

import qualified Language.Java.Rules.PullInNegations as PullInNegations
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "PullInNegations.java"
    PullInNegations.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1)
  ]
