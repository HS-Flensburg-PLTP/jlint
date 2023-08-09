module CheckNonPrivateAttributesTests (tests) where

import qualified Language.Java.Rules.CheckNonPrivateAttributes as CheckNonPrivateAttributes
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "CheckNonPrivateAttributes.java"
    CheckNonPrivateAttributes.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 5) (4, 33),
    RDF.mkRange (5, 5) (5, 38),
    RDF.mkRange (6, 5) (6, 25)
  ]
