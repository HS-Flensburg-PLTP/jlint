module SuppressWarningsTests where

import Control.Monad.Identity (zipWithM_)
import Language.Java.Rules.SuppressWarnings as SuppressWarnings (check)
import Language.Java.Syntax (CompilationUnit)
import QualifiedIdent (QualifiedIdent (..))
import qualified RDF
import Test.HUnit
import Tests
import qualified Tests

tests :: Test
tests =
  TestList
    [ rangesTest
        expectedRanges
        "SuppressWarnings.java"
        (SuppressWarnings.check [QualifiedIdent "SuppressWarnings" "method1"]),
      rangesTest
        expectedRanges
        "SuppressWarnings.java"
        (SuppressWarnings.check [UnqualifiedIdent "method1"])
    ]

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 1) (13, 2),
    RDF.mkRange (4, 5) (7, 6),
    RDF.mkRange (11, 9) (11, 50)
  ]
