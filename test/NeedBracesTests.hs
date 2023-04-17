module NeedBracesTests where

import qualified Language.Java.Rules.NeedBraces as NeedBraces
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    13
    "/test/java/NeedBraces.java"
    NeedBraces.check
