module SimplifyBooleanReturnTests where

import qualified Language.Java.Rules.SimplifyBooleanReturn as SimplifyBooleanReturn
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    10
    "/test/java/SimplifyBooleanReturn.java"
    SimplifyBooleanReturn.check
