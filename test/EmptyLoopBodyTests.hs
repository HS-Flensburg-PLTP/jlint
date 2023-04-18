module EmptyLoopBodyTests (tests) where

import qualified Language.Java.Rules.EmptyLoopBody as EmptyLoopBody
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/EmptyLoopBody.java"
    EmptyLoopBody.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1),
    RDF.mkRange (1, 1) (1, 1)
  ]
