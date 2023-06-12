module AvoidOuterNegationsTests (tests) where

import qualified Language.Java.Rules.AvoidOuterNegations as AvoidOuterNegations
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "AvoidOuterNegations.java"
    AvoidOuterNegations.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1)
  ]
