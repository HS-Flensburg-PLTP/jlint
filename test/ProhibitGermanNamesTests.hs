module ProhibitGermanNamesTests where

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
  [ RDF.mkRange (1, 7) (1, 30),
    RDF.mkRange (1, 7) (1, 30),
    RDF.mkRange (1, 7) (1, 30),
    RDF.mkRange (2, 23) (2, 50),
    RDF.mkRange (2, 23) (2, 50),
    RDF.mkRange (2, 23) (2, 50),
    RDF.mkRange (2, 23) (2, 50),
    RDF.mkRange (2, 23) (2, 50),
    RDF.mkRange (3, 26) (3, 32),
    RDF.mkRange (5, 12) (5, 35),
    RDF.mkRange (5, 12) (5, 35),
    RDF.mkRange (5, 12) (5, 35),
    RDF.mkRange (6, 16) (6, 23),
    RDF.mkRange (7, 13) (7, 26),
    RDF.mkRange (8, 16) (8, 27),
    RDF.mkRange (9, 16) (9, 23),
    RDF.mkRange (16, 18) (16, 22),
    RDF.mkRange (16, 28) (16, 32),
    RDF.mkRange (16, 39) (16, 43),
    RDF.mkRange (17, 20) (17, 37),
    RDF.mkRange (17, 20) (17, 37)
  ]
