module NoLoopBreakTests (tests) where

import qualified Language.Java.Rules.NoLoopBreak as NoLoopBreak
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoLoopBreak.java"
    NoLoopBreak.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1)
  ]
