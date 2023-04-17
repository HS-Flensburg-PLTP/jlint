module NamingConventionsTests where

import qualified Language.Java.Rules.NamingConventions as NamingConventions
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    28
    "/test/java/NamingConventions.java"
    NamingConventions.check
