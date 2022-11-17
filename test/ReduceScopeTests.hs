module ReduceScopeTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.ReduceScope as ReduceScope (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit

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
  let file = "/test/java/ReduceScope.java"
   in test [file ~: withFile file reduceScope]

reduceScope :: CompilationUnit -> FilePath -> Assertion
reduceScope cUnit path = do
  let diagnostic = ReduceScope.check cUnit path
  assertEqual "Check number of messages" 6 (length diagnostic)
  let expectedRange1 =
        Just
          ( Range
              { start = Position {line = 29, column = Just 9},
                end = Just (Position {line = 36, column = Just 9})
              }
          )
  let expectedRange2 =
        Just
          ( Range
              { start = Position {line = 41, column = Just 9},
                end = Just (Position {line = 48, column = Just 9})
              }
          )
  let expectedRange3 =
        Just
          ( Range
              { start = Position {line = 56, column = Just 10},
                end = Just (Position {line = 75, column = Just 10})
              }
          )
  let expectedRange4 =
        Just
          ( Range
              { start = Position {line = 87, column = Just 9},
                end = Just (Position {line = 98, column = Just 9})
              }
          )
  let expectedRange5 =
        Just
          ( Range
              { start = Position {line = 115, column = Just 9},
                end = Just (Position {line = 125, column = Just 9})
              }
          )
  let expectedRange6 =
        Just
          ( Range
              { start = Position {line = 130, column = Just 9},
                end = Just (Position {line = 141, column = Just 9})
              }
          )
  let expectedRanges = [expectedRange1, expectedRange2, expectedRange3, expectedRange4, expectedRange5, expectedRange6]
  zipWithM_ (assertEqual "Check range") expectedRanges (map (range . location) diagnostic)
