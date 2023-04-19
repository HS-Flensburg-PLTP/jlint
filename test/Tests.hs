module Tests (goodFileTest, rangesTest) where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit

rangesTest :: [RDF.Range] -> FilePath -> (CompilationUnit -> FilePath -> [RDF.Diagnostic]) -> Test
rangesTest testRanges =
  ruleTest (justifyRanges testRanges)

goodFileTest :: FilePath -> (CompilationUnit -> FilePath -> [RDF.Diagnostic]) -> Test
goodFileTest =
  ruleTest (assertEqual "Check number of messages" 0 . length)

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

justifyRanges :: [RDF.Range] -> [RDF.Diagnostic] -> Assertion
justifyRanges expectedRanges diagnostic = do
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
