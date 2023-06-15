module ProhibitGermanNamesTests where

import Control.Monad.Identity (zipWithM_)
import GHC.IO (unsafePerformIO)
import Language.Java.Rules.ProhibitGermanNames as ProhibitGermanNames (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesIOTest
    expectedRanges
    "ProhibitGermanNames.java"
    ProhibitGermanNames.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 7) (1, 10),
    RDF.mkRange (1, 16) (1, 23),
    RDF.mkRange (1, 24) (1, 29),
    RDF.mkRange (5, 12) (5, 15),
    RDF.mkRange (5, 21) (5, 28),
    RDF.mkRange (5, 29) (5, 34),
    RDF.mkRange (10, 18) (10, 21),
    RDF.mkRange (10, 28) (10, 31),
    RDF.mkRange (10, 39) (10, 42),
    RDF.mkRange (11, 20) (11, 24),
    RDF.mkRange (11, 25) (11, 36)
  ]
