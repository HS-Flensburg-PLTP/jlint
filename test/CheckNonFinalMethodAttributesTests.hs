module CheckNonFinalMethodAttributesTests where

import qualified Language.Java.Rules.CheckNonFinalMethodAttributes as CheckNonFinalMethodAttributes
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    1
    "/test/java/CheckNonFinalMethodAttributes.java"
    CheckNonFinalMethodAttributes.check
