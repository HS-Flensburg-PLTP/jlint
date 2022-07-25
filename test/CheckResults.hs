module CheckResults where

import RDF
import System.Directory
import Test.HUnit hiding (path)

{-
Purpose of this script is to execute the actuall asserion.
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
