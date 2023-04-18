module UseAssignOpTests where

import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/UseAssignOp.java"
    UseAssignOp.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (11, 17) (11, 46)]
