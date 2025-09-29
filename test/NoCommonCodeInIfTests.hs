module NoCommonCodeInIfTests where

import qualified Language.Java.Rules.NoCommonCodeInIf as NoCommonCodeInIf
import qualified RDF
import Test.HUnit
import Tests (rangesTest)

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoCommonCodeInIf.java"
    NoCommonCodeInIf.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (9, 9) (15, 10),
    RDF.mkRange (17, 9) (21, 10),
    RDF.mkRange (23, 9) (27, 19),
    RDF.mkRange (38, 9) (45, 10),
    RDF.mkRange (49, 9) (55, 10),
    RDF.mkRange (71, 9) (78, 10),
    RDF.mkRange (71, 9) (78, 10),
    RDF.mkRange (82, 9) (92, 10),
    RDF.mkRange (125, 9) (131, 10)
  ]
