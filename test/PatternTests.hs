module PatternTests (tests) where

import qualified Language.Java.Rules.Pattern as Pattern
import qualified Markdown
import QualifiedIdent (QualifiedIdent (..))
import qualified RDF
import Test.HUnit (Test (..), Testable (test))
import Tests

tests :: Test
tests =
  TestList
    [ rangesTest
        expectedRanges1
        "Pattern.java"
        (Pattern.check (Pattern.Method "nodeAt" [0]) ""),
      rangesTest
        expectedRanges2
        "Pattern.java"
        (Pattern.check (Pattern.Invocation "first" "prev") ""),
      rangesTest
        expectedRanges3
        "Pattern.java"
        (Pattern.check (Pattern.Invocation "last" "next") "")
    ]

expectedRanges1 :: [RDF.Range]
expectedRanges1 =
  [ RDF.mkRange (4, 25) (4, 35),
    RDF.mkRange (5, 21) (5, 30)
  ]

expectedRanges2 :: [RDF.Range]
expectedRanges2 =
  [ RDF.mkRange (7, 28) (7, 35),
    RDF.mkRange (8, 18) (8, 30)
  ]

expectedRanges3 :: [RDF.Range]
expectedRanges3 =
  [ RDF.mkRange (9, 27) (9, 34),
    RDF.mkRange (10, 18) (10, 29)
  ]
