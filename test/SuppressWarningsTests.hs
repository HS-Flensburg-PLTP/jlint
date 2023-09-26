module SuppressWarningsTests where

import Config (QualifiedIdent (QualifiedIdent))
import Control.Monad.Identity (zipWithM_)
import Language.Java.Rules.SuppressWarnings as SuppressWarnings (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests
import qualified Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "SuppressWarnings.java"
    (SuppressWarnings.check [QualifiedIdent "SuppressWarnings" "method1"])

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (13, 2),
    RDF.mkRange (4, 5) (7, 6),
    RDF.mkRange (11, 9) (11, 50)
  ]
