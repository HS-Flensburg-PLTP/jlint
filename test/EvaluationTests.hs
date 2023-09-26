module EvaluationTests where

import qualified Language.Java.Rules.Evaluation as Evaluation
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "Evaluation.java"
    Evaluation.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (19, 21) (19, 30)
  ]
