module NoIncDecInExpressionTests where

import qualified Language.Java.Rules.NoIncDecInExpression as NoIncDecInExpression
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoIncDecInExpression.java"
    NoIncDecInExpression.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (3, 21) (3, 24),
    RDF.mkRange (6, 17) (6, 24),
    RDF.mkRange (6, 18) (6, 21),
    RDF.mkRange (9, 15) (9, 18),
    RDF.mkRange (11, 22) (11, 25),
    RDF.mkRange (12, 47) (12, 50),
    RDF.mkRange (12, 56) (12, 59),
    RDF.mkRange (12, 63) (12, 66),
    RDF.mkRange (17, 13) (17, 16),
    RDF.mkRange (17, 20) (17, 23),
    RDF.mkRange (18, 18) (18, 21),
    RDF.mkRange (24, 13) (24, 16)
  ]
