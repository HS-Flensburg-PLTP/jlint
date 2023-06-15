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
  [ RDF.mkRange (6, 16) (6, 58),
    RDF.mkRange (14, 16) (14, 55),
    RDF.mkRange (22, 16) (22, 31),
    RDF.mkRange (30, 13) (30, 28)
  ]
