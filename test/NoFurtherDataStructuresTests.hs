module NoFurtherDataStructuresTests (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Language.Java.Rules.NoFurtherDataStructures as NoFurtherDataStructures
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoFurtherDataStructures.java"
    (NoFurtherDataStructures.check ("set" :| ["get", "pop"]))

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (5, 5) (10, 6),
    RDF.mkRange (19, 5) (23, 6),
    RDF.mkRange (33, 5) (38, 6)
  ]
