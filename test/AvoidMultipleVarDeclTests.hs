module AvoidMultipleVarDeclTests (tests) where

import qualified Language.Java.Rules.AvoidMultipleVarDecl as AvoidMultipleVarDecl
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "AvoidMultipleVarDecl.java"
    AvoidMultipleVarDecl.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (6, 9) (6, 18),
    RDF.mkRange (8, 9) (9, 15),
    RDF.mkRange (11, 9) (11, 30)
  ]
