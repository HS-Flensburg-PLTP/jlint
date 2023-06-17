module NoPostIncDecInExpressionTests where

import qualified Language.Java.Rules.NoPostIncDecInExpression as NoPostIncDecInExpression
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoPostIncDecInExpression.java"
    NoPostIncDecInExpression.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (3, 22) (3, 24),
    RDF.mkRange (6, 22) (6, 24),
    RDF.mkRange (6, 19) (6, 21),
    RDF.mkRange (9, 16) (9, 18),
    RDF.mkRange (11, 23) (11, 25),
    RDF.mkRange (12, 48) (12, 50),
    RDF.mkRange (12, 57) (12, 59),
    RDF.mkRange (12, 64) (12, 66),
    RDF.mkRange (17, 14) (17, 16),
    RDF.mkRange (17, 21) (17, 23),
    RDF.mkRange (18, 19) (18, 21),
    RDF.mkRange (24, 14) (24, 16)
  ]
