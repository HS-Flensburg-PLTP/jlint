module DefaultComesLastTests where

import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
import Test.HUnit (Test)
import Tests

tests :: Test
tests =
  countTest
    1
    "/test/java/DefaultComesLast.java"
    DefaultComesLast.check
