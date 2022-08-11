module CheckResults where

import RDF
import System.Directory
import System.FilePath.Posix (takeBaseName)
import Test.HUnit hiding (path)

{-
This script is dedicated to the actuall execution of assertions.
Each function expects a list of diagnostic Results as well as the expected value.
-}
-- TODO: compareLists needs path to getFilename. It would be more suiteable to change the bracket pattern in RessourceManger.withCunit to work with a more flexible datatype than : [IO (CompilationUnit, FilePath)]
compareLists :: Show a => Eq a => (Diagnostic -> a) -> [Diagnostic] -> a -> String -> IO ()
compareLists attrField diags expected testFilePath =
  assertEqual testFilePath [expected] (map attrField diags)

checkPath :: [Diagnostic] -> FilePath -> IO ()
checkPath diag testFilePath = do
  compareLists (path . location) diag testFilePath testFilePath

checkMessage :: [Diagnostic] -> String -> FilePath -> IO ()
checkMessage =
  compareLists message
