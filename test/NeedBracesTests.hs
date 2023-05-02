module NeedBracesTests (tests) where

import qualified Language.Java.Rules.NeedBraces as NeedBraces
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NeedBraces.java"
    NeedBraces.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 37) (4, 59),
    RDF.mkRange (6, 9) (6, 27),
    RDF.mkRange (8, 12) (8, 16),
    RDF.mkRange (11, 13) (12, 21),
    RDF.mkRange (12, 17) (12, 21),
    RDF.mkRange (17, 13) (17, 17),
    RDF.mkRange (28, 13) (28, 33),
    RDF.mkRange (30, 13) (30, 17)
  ]
