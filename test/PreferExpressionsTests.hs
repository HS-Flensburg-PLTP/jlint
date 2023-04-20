module PreferExpressionsTests (tests) where

import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "PreferExpressions.java"
    PreferExpressions.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (7, 13) (8, 22),
    RDF.mkRange (8, 13) (9, 48),
    RDF.mkRange (9, 13) (10, 49),
    RDF.mkRange (10, 13) (10, 49),
    RDF.mkRange (19, 13) (20, 50),
    RDF.mkRange (20, 13) (21, 51),
    RDF.mkRange (21, 13) (22, 23),
    RDF.mkRange (22, 13) (22, 23),
    RDF.mkRange (32, 13) (33, 51),
    RDF.mkRange (33, 13) (34, 23),
    RDF.mkRange (34, 13) (34, 23),
    RDF.mkRange (43, 13) (44, 53),
    RDF.mkRange (44, 13) (45, 54),
    RDF.mkRange (45, 13) (45, 54),
    RDF.mkRange (55, 13) (56, 23),
    RDF.mkRange (63, 10) (64, 54),
    RDF.mkRange (78, 13) (78, 26),
    RDF.mkRange (102, 13) (102, 63)
  ]
