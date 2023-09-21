module NoExtraDataStructuresTests (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Language.Java.Rules.NoExtraDataStructures as NoExtraDataStructures
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoExtraDataStructures.java"
    (NoExtraDataStructures.check ("set" :| ["get", "pop", "shiftElements"]))

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (5, 5) (10, 6),
    RDF.mkRange (22, 5) (26, 6),
    RDF.mkRange (39, 5) (44, 6),
    RDF.mkRange (52, 5) (57, 6)
  ]
