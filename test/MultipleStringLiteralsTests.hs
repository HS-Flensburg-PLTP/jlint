module MultipleStringLiteralsTests where

import Control.Monad (zipWithM_)
import qualified Language.Java.Rules.MultipleStringLiterals as MultipleStringLiterals
import Language.Java.Syntax
import qualified RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  Tests.rangesTest
    expectedRanges
    "MultipleStringLiterals.java"
    MultipleStringLiterals.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (2, 24) (2, 29),
    RDF.mkRange (3, 24) (3, 29),
    RDF.mkRange (4, 24) (4, 29),
    RDF.mkRange (7, 13) (7, 18),
    RDF.mkRange (8, 13) (8, 18),
    RDF.mkRange (13, 13) (13, 18),
    RDF.mkRange (13, 21) (13, 26)
  ]
