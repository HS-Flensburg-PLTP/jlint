module DeclarationOrderTests where

import qualified Language.Java.Rules.DeclarationOrder as DeclarationOrder
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "DeclarationOrder.java"
    DeclarationOrder.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (5, 5) (5, 18),
    RDF.mkRange (14, 5) (16, 6),
    RDF.mkRange (18, 5) (18, 25),
    RDF.mkRange (23, 9) (23, 22),
    RDF.mkRange (32, 9) (34, 10),
    RDF.mkRange (36, 9) (36, 29),
    RDF.mkRange (42, 9) (42, 22),
    RDF.mkRange (51, 9) (53, 10),
    RDF.mkRange (55, 9) (55, 29)
  ]
