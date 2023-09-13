module NamingConventionsTests (tests) where

import qualified Language.Java.Rules.NamingConventions as NamingConventions
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NamingConventions.java"
    NamingConventions.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (1, 9) (1, 11),
    RDF.mkRange (43, 10) (43, 16),
    RDF.mkRange (44, 10) (44, 17),
    RDF.mkRange (45, 10) (45, 17),
    RDF.mkRange (47, 21) (47, 26),
    RDF.mkRange (47, 32) (47, 38),
    RDF.mkRange (47, 44) (47, 50),
    RDF.mkRange (24, 16) (24, 22),
    RDF.mkRange (25, 16) (25, 23),
    RDF.mkRange (26, 16) (26, 23),
    RDF.mkRange (34, 19) (34, 27),
    RDF.mkRange (35, 19) (35, 28),
    RDF.mkRange (36, 19) (36, 28),
    RDF.mkRange (38, 13) (38, 16),
    RDF.mkRange (39, 13) (39, 18),
    RDF.mkRange (40, 13) (40, 17),
    RDF.mkRange (28, 9) (28, 15),
    RDF.mkRange (29, 9) (29, 16),
    RDF.mkRange (30, 9) (30, 16),
    RDF.mkRange (3, 7) (3, 17),
    RDF.mkRange (4, 11) (4, 26),
    RDF.mkRange (5, 7) (5, 18),
    RDF.mkRange (6, 11) (6, 26),
    RDF.mkRange (7, 7) (7, 18),
    RDF.mkRange (8, 11) (8, 26),
    RDF.mkRange (13, 10) (13, 20),
    RDF.mkRange (16, 10) (16, 20),
    RDF.mkRange (19, 10) (19, 20)
  ]
