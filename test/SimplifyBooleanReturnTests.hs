module SimplifyBooleanReturnTests where

import qualified Language.Java.Rules.SimplifyBooleanReturn as SimplifyBooleanReturn
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/SimplifyBooleanReturn.java"
    SimplifyBooleanReturn.check

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
    RDF.mkRange (1, 1) (1, 1)
  ]
