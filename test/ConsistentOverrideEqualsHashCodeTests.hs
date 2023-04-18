module ConsistentOverrideEqualsHashCodeTests (tests) where

import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified RDF
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "/test/java/ConsistentOverrideEqualsHashCode.java"
    ConsistentOverrideEqualsHashCode.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (2, 5) (4, 6),
    RDF.mkRange (10, 5) (12, 6),
    RDF.mkRange (40, 5) (42, 6),
    RDF.mkRange (45, 5) (47, 6)
  ]
