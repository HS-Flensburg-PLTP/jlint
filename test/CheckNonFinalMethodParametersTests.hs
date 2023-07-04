module CheckNonFinalMethodParametersTests (tests) where

import qualified Language.Java.Rules.CheckNonFinalMethodParameters as CheckNonFinalMethodParameters
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "CheckNonFinalMethodParameters.java"
    CheckNonFinalMethodParameters.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (2, 40) (2, 47),
    RDF.mkRange (3, 21) (3, 32),
    RDF.mkRange (4, 21) (4, 33),
    RDF.mkRange (4, 35) (4, 47)
  ]
