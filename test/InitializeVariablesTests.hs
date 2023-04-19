module InitializeVariablesTests (tests) where

import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/InitializeVariables.java"
    InitializeVariables.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (8, 13) (8, 64)]
