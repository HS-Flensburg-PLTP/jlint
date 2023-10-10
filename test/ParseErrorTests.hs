module ParseErrorTests (tests) where

import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "ParseError.java"
    (\_ _ -> [])

expectedRanges :: [RDF.Range]
expectedRanges =
  [RDF.mkRange (3, 24) (3, 24)]
