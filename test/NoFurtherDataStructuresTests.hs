module NoFurtherDataStructuresTests (tests) where

import qualified Language.Java.Rules.NoFurtherDataStructures as NoFurtherDataStructures
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoFurtherDataStructures.java"
    (NoFurtherDataStructures.check ["set", "get", "pop"])

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (5, 5) (9, 6),
    RDF.mkRange (18, 5) (22, 6),
    RDF.mkRange (32, 5) (36, 6)
  ]
