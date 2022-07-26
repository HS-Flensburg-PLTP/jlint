module CheckResults where

import RDF
import System.Directory
import Test.HUnit hiding (path)

{-
This script is dedicated to the actuall execution of assertions.
Each function expects a list of diagnostic Results as well as the expected value.
-}
compareLists :: Show a => Eq a => (Diagnostic -> a) -> [Diagnostic] -> a -> IO ()
compareLists attrField diags expected =
  [expected] @=? map attrField diags

checkPath :: [Diagnostic] -> String -> IO ()
checkPath =
  compareLists (path . location)

checkMessage :: [Diagnostic] -> String -> IO ()
checkMessage =
  compareLists message
