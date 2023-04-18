module SameExecutionsInIfTests (tests) where

import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  rangesTest
    [] -- rule seems broken
    "/test/java/SameExecutionsInIf.java"
    SameExecutionsInIf.check
