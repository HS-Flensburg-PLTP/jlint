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
    RDF.mkRange (14, 5) (16, 10),
    RDF.mkRange (18, 5) (18, 25)
  ]
