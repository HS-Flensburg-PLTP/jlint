module MethodInvocationsTests where

import qualified Language.Java.Rules.MethodInvocations as MethodInvocations
import QualifiedIdent (QualifiedIdent (..))
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "MethodInvocations.java"
    (MethodInvocations.check (QualifiedIdent "MethodInvocations" "foo") "bar" 2 "Dies ist ein Test")

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (8, 5) (10, 6)
  ]
