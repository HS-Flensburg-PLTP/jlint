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
    RDF.mkRange (1, 31) (1, 37),
    RDF.mkRange (2, 23) (2, 50),
    RDF.mkRange (3, 26) (3, 32),
    RDF.mkRange (5, 20) (5, 25),
    RDF.mkRange (8, 16) (8, 23),
    RDF.mkRange (9, 13) (9, 26),
    RDF.mkRange (10, 16) (10, 27),
    RDF.mkRange (11, 16) (11, 23),
    RDF.mkRange (18, 18) (18, 22),
    RDF.mkRange (19, 20) (19, 37),
    RDF.mkRange (23, 6) (23, 19),
    RDF.mkRange (26, 5) (26, 9),
    RDF.mkRange (28, 11) (28, 24)
  ]
