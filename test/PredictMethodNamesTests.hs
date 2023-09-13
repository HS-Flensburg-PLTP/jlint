module PredictMethodNamesTests where

import Language.Java.Rules.PredictMethodNames as PredictMethodNames (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesIOTest
    expectedRanges
    "PredictMethodNames.java"
    (PredictMethodNames.check ["powerOf", "faculty"])

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (2, 9) (2, 27)]
