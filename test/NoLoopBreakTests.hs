module NoLoopBreakTests where

import qualified Language.Java.Rules.NoLoopBreak as NoLoopBreak
import Test.HUnit
import Tests

tests :: Test
tests =
  countTest
    12
    "/test/java/NoLoopBreak.java"
    NoLoopBreak.check
