module AvoidStaticImportTests (tests) where

import qualified Language.Java.Rules.AvoidStaticImport as AvoidStaticImport
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "AvoidStaticImport.java"
    AvoidStaticImport.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (1, 34),
    RDF.mkRange (2, 1) (2, 34)
  ]
