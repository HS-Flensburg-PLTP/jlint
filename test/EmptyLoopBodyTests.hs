module EmptyLoopBodyTests where

import qualified Language.Java.Rules.EmptyLoopBody as EmptyLoopBody
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    8
    "/test/java/EmptyLoopBody.java"
    EmptyLoopBody.check
