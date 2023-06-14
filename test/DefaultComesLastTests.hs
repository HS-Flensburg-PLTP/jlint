module DefaultComesLastTests (tests) where

import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "DefaultComesLast.java"
    DefaultComesLast.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (7, 13) (8, 23),
    RDF.mkRange (17, 9) (18, 19)
  ]
