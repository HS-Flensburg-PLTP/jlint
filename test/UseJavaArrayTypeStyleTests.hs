module UseJavaArrayTypeStyleTests where

import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/UseJavaArrayTypeStyle.java"
    UseJavaArrayTypeStyle.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (3, 12) (3, 20),
    RDF.mkRange (6, 13) (6, 21),
    RDF.mkRange (8, 13) (8, 23),
    RDF.mkRange (11, 23) (11, 30),
    RDF.mkRange (13, 39) (13, 45)
  ]
