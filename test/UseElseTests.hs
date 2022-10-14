module UseElseTests where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax (CompilationUnit)
import RDF
import RessourceManager
import System.Directory (getCurrentDirectory)
import Test.HUnit
import UseElse (check)

withFile :: FilePath -> (CompilationUnit -> FilePath -> Assertion) -> Assertion
withFile relativePath check = do
  path <- getCurrentDirectory
  let file = path ++ relativePath
  content <- readFile file
  case parser compilationUnit file content of
    Left error ->
      assertFailure ("Parsing " ++ file ++ " failed with error:" ++ show error)
    Right cUnit ->
      check cUnit path

tests :: Test
tests =
  let file = "/test/UseElse.java"
   in test [file ~: withFile file useElse]

useElse :: CompilationUnit -> FilePath -> Assertion
useElse cUnit path = do
  let diagnostic = UseElse.check cUnit path
  let expectedMsg = "Always use an `else` if the code in the then branch always exits."
  assertEqual "Check message" [expectedMsg] (map message diagnostic)
  -- This source span should be exclusive -> adapt language-java
  let expectedRange = Just (Range {start = Position {line = 4, column = 9}, end = Just (Position {line = 6, column = 9})})
  assertEqual "Check range" [expectedRange] (map (range . location) diagnostic)
