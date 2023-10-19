module MethodInvocationsTests where

import qualified Language.Java.Rules.MethodInvocations as MethodInvocations
import QualifiedIdent (QualifiedIdent (..))
import qualified RDF
import Test.HUnit
import Tests

tests :: Test
tests =
  TestList
    [ rangesTest
        expectedRanges1
        "MethodInvocations.java"
        (MethodInvocations.check (QualifiedIdent "MethodInvocations" "foo") "bar" 2 ""),
      rangesTest
        expectedRanges2
        "MethodInvocations.java"
        (MethodInvocations.check (QualifiedIdent "MethodInvocations" "remove") "nodeAt" 1 "")
    ]

expectedRanges1 :: [RDF.Range]
expectedRanges1 =
  [ RDF.mkRange (8, 5) (10, 6)
  ]

expectedRanges2 :: [RDF.Range]
expectedRanges2 =
  [ RDF.mkRange (24, 5) (38, 6)
  ]
