module Tests (rangesTest, rangesIOTest) where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax (CompilationUnit, Parsed)
import qualified RDF
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.HUnit

javaTestDirectory :: FilePath
javaTestDirectory = "test" </> "java"

rangesTest :: [RDF.Range] -> FilePath -> (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]) -> Test
rangesTest testRanges path check =
  rangesIOTest testRanges path (\cUnit path -> return (check cUnit path))

rangesIOTest :: [RDF.Range] -> FilePath -> (CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]) -> Test
rangesIOTest testRanges =
  ruleIOTest (justifyRanges testRanges)

ruleIOTest :: ([RDF.Diagnostic] -> Assertion) -> FilePath -> (CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]) -> Test
ruleIOTest justify path check =
  path ~: do
    dir <- getCurrentDirectory
    let file = dir </> javaTestDirectory </> path
    content <- readFile file
    case parser compilationUnit file content of
      Left error ->
        assertFailure ("Parsing " ++ file ++ " failed with error:" ++ show error)
      Right cUnit -> do
        justify <$> check cUnit path

justifyRanges :: [RDF.Range] -> [RDF.Diagnostic] -> Assertion
justifyRanges expectedRanges diagnostic = do
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
