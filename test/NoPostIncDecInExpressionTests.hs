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
    RDF.mkRange (7, 23) (7, 25)
  ]
