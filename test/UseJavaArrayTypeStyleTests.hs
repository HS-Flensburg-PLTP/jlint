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
    RDF.mkRange (6, 13) (6, 22),
    RDF.mkRange (10, 23) (10, 30),
    RDF.mkRange (11, 13) (11, 24),
    RDF.mkRange (14, 39) (14, 45)
  ]
