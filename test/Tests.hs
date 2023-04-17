module Tests (countTest, rangesTest, withParsedJavaFile) where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit

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

ruleTest :: ([RDF.Diagnostic] -> Assertion) -> FilePath -> (CompilationUnit -> FilePath -> [RDF.Diagnostic]) -> Test
ruleTest justify path check =
  path ~: do
    dir <- getCurrentDirectory
    let file = dir ++ path
    content <- readFile file
    case parser compilationUnit file content of
      Left error ->
        assertFailure ("Parsing " ++ file ++ " failed with error:" ++ show error)
      Right cUnit ->
        justify (check cUnit path)

-- "quick fix" for old tests without rangeDiagnostics
countTest :: Int -> FilePath -> (CompilationUnit -> FilePath -> [RDF.Diagnostic]) -> Test
countTest expectedCount =
  ruleTest (assertEqual "Check number of messages" expectedCount . length)

rangesTest :: [RDF.Range] -> FilePath -> (CompilationUnit -> FilePath -> [RDF.Diagnostic]) -> Test
rangesTest testRanges =
  ruleTest (justifyRanges testRanges)

justifyRanges :: [RDF.Range] -> [RDF.Diagnostic] -> Assertion
justifyRanges expectedRanges diagnostic = do
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
