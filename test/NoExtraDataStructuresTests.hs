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
  [ RDF.mkRange (8, 25) (8, 38),
    RDF.mkRange (9, 26) (9, 39),
    RDF.mkRange (25, 25) (25, 38),
    RDF.mkRange (40, 29) (40, 47),
    RDF.mkRange (43, 27) (43, 45),
    RDF.mkRange (53, 21) (53, 34)
  ]
