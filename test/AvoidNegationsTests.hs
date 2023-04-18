module AvoidNegationsTests where

import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified RDF
import Test.HUnit (Test, Testable (test))
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/AvoidNegations.java"
    AvoidNegations.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (6, 9) (10, 10),
    RDF.mkRange (15, 9) (18, 37),
    RDF.mkRange (22, 9) (28, 10),
    RDF.mkRange (42, 9) (46, 10),
    RDF.mkRange (50, 9) (54, 10),
    RDF.mkRange (60, 13) (64, 14),
    RDF.mkRange (72, 24) (72, 81)
  ]
