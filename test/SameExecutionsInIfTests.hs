module SameExecutionsInIfTests where

import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    0 -- rule seems broken
    "/test/java/SameExecutionsInIf.java"
    SameExecutionsInIf.check
