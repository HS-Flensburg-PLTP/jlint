module MethodInvNumberTests where

import Config (MethodLimit (MethodLimit))
import qualified Language.Java.Rules.MethodInvNumber as MethodInvNumber
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "MethodInvNumber.java"
    (MethodInvNumber.check [MethodLimit "foo" "bar" 2])

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (8, 5) (10, 6)
  ]
