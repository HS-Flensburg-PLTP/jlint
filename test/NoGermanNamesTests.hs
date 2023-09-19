module NoGermanNamesTests where

import Language.Java.Rules.NoGermanNames as NoGermanNames (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesIOTest
    expectedRanges
    "NoGermanNames.java"
    NoGermanNames.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 7) (1, 30),
    RDF.mkRange (1, 31) (1, 36),
    RDF.mkRange (2, 23) (2, 50),
    RDF.mkRange (3, 26) (3, 33),
    RDF.mkRange (5, 19) (5, 24),
    RDF.mkRange (8, 16) (8, 23),
    RDF.mkRange (9, 13) (9, 26),
    RDF.mkRange (10, 16) (10, 27),
    RDF.mkRange (11, 16) (11, 23),
    RDF.mkRange (18, 18) (18, 22),
    RDF.mkRange (19, 20) (19, 37),
    RDF.mkRange (20, 36) (20, 43),
    RDF.mkRange (25, 6) (25, 19),
    RDF.mkRange (28, 5) (28, 9),
    RDF.mkRange (31, 11) (31, 24),
    RDF.mkRange (34, 17) (34, 33)
  ]
