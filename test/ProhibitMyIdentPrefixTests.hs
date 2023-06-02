module ProhibitMyIdentPrefixTests where

import Language.Java.Rules.ProhibitMyIdentPrefix as ProhibitMyIdentPrefix (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests
import qualified Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "ProhibitMyIdentPrefix.java"
    ProhibitMyIdentPrefix.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 9) (1, 11),
    RDF.mkRange (4, 16) (4, 18),
    RDF.mkRange (7, 14) (7, 21),
    RDF.mkRange (8, 22) (8, 29),
    RDF.mkRange (10, 12) (10, 20),
    RDF.mkRange (14, 10) (14, 16),
    RDF.mkRange (15, 9) (15, 20),
    RDF.mkRange (17, 9) (17, 20),
    RDF.mkRange (20, 12) (20, 19),
    RDF.mkRange (21, 16) (21, 31),
    RDF.mkRange (23, 18) (23, 27),
    RDF.mkRange (23, 33) (23, 42),
    RDF.mkRange (23, 49) (23, 58),
    RDF.mkRange (24, 17) (24, 27),
    RDF.mkRange (27, 14) (27, 20),
    RDF.mkRange (29, 9) (29, 15),
    RDF.mkRange (29, 25) (29, 35),
    RDF.mkRange (34, 17) (34, 25),
    RDF.mkRange (35, 16) (35, 32),
    RDF.mkRange (38, 17) (38, 32),
    RDF.mkRange (39, 23) (39, 34),
    RDF.mkRange (40, 18) (40, 29),
    RDF.mkRange (41, 17) (41, 34)
  ]
