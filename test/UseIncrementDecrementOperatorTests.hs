module UseIncrementDecrementOperatorTests (tests) where

import qualified Language.Java.Rules.UseIncrementDecrementOperator as UseIncrementDecrementOperator
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "UseIncrementDecrementOperator.java"
    UseIncrementDecrementOperator.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 9) (4, 18),
    RDF.mkRange (5, 9) (5, 18),
    RDF.mkRange (6, 9) (6, 18),
    RDF.mkRange (7, 9) (7, 15),
    RDF.mkRange (9, 35) (9, 44),
    RDF.mkRange (12, 35) (12, 41),
    RDF.mkRange (15, 16) (15, 25),
    RDF.mkRange (28, 9) (28, 23)
  ]
