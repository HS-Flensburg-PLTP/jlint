module NeedBracesTests (tests) where

import qualified Language.Java.Rules.NeedBraces as NeedBraces
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NeedBraces.java"
    NeedBraces.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (4, 37) (4, 59),
    RDF.mkRange (7, 13) (7, 35),
    RDF.mkRange (9, 12) (9, 16),
    RDF.mkRange (12, 13) (13, 21),
    RDF.mkRange (13, 17) (13, 21),
    RDF.mkRange (1, 1) (1, 1), -- noch keine SourceSpan für `Empty`
    RDF.mkRange (18, 13) (18, 17),
    RDF.mkRange (1, 1) (1, 1), -- noch keine SourceSpan für `Empty`
    RDF.mkRange (29, 13) (29, 33),
    RDF.mkRange (31, 13) (31, 17)
  ]
