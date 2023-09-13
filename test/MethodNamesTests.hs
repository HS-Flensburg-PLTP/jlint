module MethodNamesTests where

import Language.Java.Rules.MethodNames as MethodNames (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesIOTest
    expectedRanges
    "MethodNames.java"
    (MethodNames.check ["powerOf", "faculty"])

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (2, 9) (2, 27)]
