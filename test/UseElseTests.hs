module UseElseTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.UseElse (check)
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
  let file = "/test/java/UseElse.java"
   in test [file ~: withFile file useElse]

useElse :: CompilationUnit -> FilePath -> Assertion
useElse cUnit path = do
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" 6 (length diagnostic)
  let expectedRange1 =
        Just
          ( Range
              { start = Position {line = 4, column = Just 9},
                end = Just (Position {line = 6, column = Just 9})
              }
          )
  let expectedRange2 =
        Just
          ( Range
              { start = Position {line = 11, column = Just 10},
                end = Just (Position {line = 13, column = Just 10})
              }
          )
  let expectedRange3 =
        Just
          ( Range
              { start = Position {line = 14, column = Just 10},
                end = Just (Position {line = 21, column = Just 10})
              }
          )
  let expectedRange4 =
        Just
          ( Range
              { start = Position {line = 30, column = Just 9},
                end = Just (Position {line = 32, column = Just 9})
              }
          )
  let expectedRange5 =
        Just
          ( Range
              { start = Position {line = 41, column = Just 9},
                end = Just (Position {line = 46, column = Just 9})
              }
          )
  let expectedRange6 =
        Just
          ( Range
              { start = Position {line = 52, column = Just 9},
                end = Just (Position {line = 58, column = Just 9})
              }
          )
  let expectedRanges = [expectedRange1, expectedRange2, expectedRange3, expectedRange4, expectedRange5, expectedRange6]
  zipWithM_ (assertEqual "Check range") expectedRanges (map (range . location) diagnostic)
