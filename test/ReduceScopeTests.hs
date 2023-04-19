module ReduceScopeTests (tests) where

import Language.Java.Rules.ReduceScope as ReduceScope
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/ReduceScope.java"
    ReduceScope.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (29, 9) (36, 10),
    RDF.mkRange (41, 9) (48, 10),
    RDF.mkRange (56, 10) (75, 11),
    RDF.mkRange (56, 10) (75, 11),
    RDF.mkRange (56, 10) (75, 11),
    RDF.mkRange (87, 9) (98, 10),
    RDF.mkRange (115, 9) (125, 10),
    RDF.mkRange (130, 9) (141, 10),
    RDF.mkRange (172, 9) (178, 10)
  ]
