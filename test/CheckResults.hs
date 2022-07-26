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

checkPath :: [Diagnostic] -> FilePath -> IO ()
checkPath diag patht = do
  print (head diag)
  compareLists (path . location) diag patht

checkMessage :: [Diagnostic] -> String -> IO ()
checkMessage =
  compareLists message
