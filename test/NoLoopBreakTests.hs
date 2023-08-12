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
  [ RDF.mkRange (9, 20) (9, 26),
    RDF.mkRange (20, 17) (20, 23),
    RDF.mkRange (32, 13) (32, 19),
    RDF.mkRange (37, 13) (37, 19),
    RDF.mkRange (42, 13) (42, 19),
    RDF.mkRange (47, 13) (47, 19),
    RDF.mkRange (53, 17) (53, 23),
    RDF.mkRange (60, 17) (60, 23),
    RDF.mkRange (68, 13) (68, 20),
    RDF.mkRange (73, 13) (73, 20),
    RDF.mkRange (78, 13) (78, 20),
    RDF.mkRange (83, 13) (83, 20),
    RDF.mkRange (89, 17) (89, 24),
    RDF.mkRange (96, 17) (96, 24)
  ]
