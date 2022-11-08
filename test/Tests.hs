module Tests (withParsedJavaFile) where

import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax (CompilationUnit)
import System.Directory (getCurrentDirectory)
import Test.HUnit (Assertion, assertFailure)

withParsedJavaFile :: FilePath -> (CompilationUnit -> FilePath -> Assertion) -> Assertion
withParsedJavaFile relativePath check = do
  path <- getCurrentDirectory
  let file = path ++ relativePath
  content <- readFile file
  case parser compilationUnit file content of
    Left error ->
      assertFailure ("Parsing " ++ file ++ " failed with error:" ++ show error)
    Right cUnit ->
      check cUnit path
