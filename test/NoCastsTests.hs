module NoCastsTests (tests) where

import qualified Language.Java.Rules.NoCasts as NoCasts
import qualified RDF
import Test.HUnit (Test, Testable (test))
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoCasts.java"
    (NoCasts.check ["foo"])

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (8, 5) (11, 6)]
