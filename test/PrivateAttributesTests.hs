module PrivateAttributesTests (tests) where

import qualified Language.Java.Rules.PrivateAttributes as PrivateAttributes
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "PrivateAttributes.java"
    PrivateAttributes.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 5) (4, 33),
    RDF.mkRange (5, 5) (5, 38),
    RDF.mkRange (6, 5) (6, 25),
    RDF.mkRange (7, 5) (7, 18),
    RDF.mkRange (8, 5) (8, 27)
  ]
