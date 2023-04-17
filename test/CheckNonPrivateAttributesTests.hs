module CheckNonPrivateAttributesTests where

import qualified Language.Java.Rules.CheckNonPrivateAttributes as CheckNonPrivateAttributes
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    3
    "/test/java/CheckNonPrivateAttributes.java"
    CheckNonPrivateAttributes.check
