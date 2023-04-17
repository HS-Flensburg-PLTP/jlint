module CheckNonFinalMethodAttributesTests where

import qualified Language.Java.Rules.CheckNonFinalMethodAttributes as CheckNonFinalMethodAttributes
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/CheckNonFinalMethodAttributes.java"
    CheckNonFinalMethodAttributes.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (1, 1) (1, 1)]
