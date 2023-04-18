module UseElseTests where

import qualified Language.Java.Rules.UseElse as UseElse
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/UseElse.java"
    UseElse.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 9) (6, 10),
    RDF.mkRange (11, 10) (13, 11),
    RDF.mkRange (14, 10) (21, 11),
    RDF.mkRange (30, 9) (32, 10),
    RDF.mkRange (41, 9) (46, 10),
    RDF.mkRange (52, 9) (58, 10),
    RDF.mkRange (83, 9) (96, 10)
  ]
