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
  [ RDF.mkRange (6, 13) (6, 18)
  ]
