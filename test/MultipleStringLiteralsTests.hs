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
  [ RDF.mkRange (8, 13) (8, 18),
    RDF.mkRange (7, 13) (7, 18),
    RDF.mkRange (11, 22) (11, 27),
    RDF.mkRange (15, 22) (15, 27)
  ]
