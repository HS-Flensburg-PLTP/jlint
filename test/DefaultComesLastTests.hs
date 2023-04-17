module DefaultComesLastTests where

import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/DefaultComesLast.java"
    DefaultComesLast.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (1, 1) (1, 1)]
