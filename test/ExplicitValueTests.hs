module ExplicitValueTests where

import qualified Language.Java.Rules.ExplicitValue as ExplicitValue
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "ExplicitValue.java"
    ExplicitValue.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (5, 60) (5, 65),
    RDF.mkRange (10, 60) (10, 65),
    RDF.mkRange (18, 60) (18, 65),
    RDF.mkRange (25, 60) (25, 65),
    RDF.mkRange (45, 60) (45, 65),
    RDF.mkRange (50, 60) (50, 65),
    RDF.mkRange (60, 60) (60, 65)
  ]
