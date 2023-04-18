module AvoidStarImportTests where

import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/AvoidStarImport.java"
    AvoidStarImport.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (1, 24),
    RDF.mkRange (2, 1) (2, 24)
  ]
