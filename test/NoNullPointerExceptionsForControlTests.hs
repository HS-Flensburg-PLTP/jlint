module NoNullPointerExceptionsForControlTests where

import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/NoNullPointerExceptionsForControl.java"
    NoNullPointerExceptionsForControl.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (7, 13) (11, 14),
    RDF.mkRange (19, 13) (23, 14)
  ]
