module PreferExpressionsTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.PreferExpressions as PreferExpressions (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/PreferExpressions.java"
   in test [file ~: Tests.withParsedJavaFile file preferExpressions]

preferExpressions :: CompilationUnit -> FilePath -> Assertion
preferExpressions cUnit path = do
  let diagnostic = PreferExpressions.check cUnit path
  assertEqual "Check number of messages" 11 (length diagnostic)
  let expectedRange1 =
        Just
          ( Range
              { start = Position {line = 7, column = Just 13},
                end = Just (Position {line = 9, column = Just 13})
              }
          )
  let expectedRange2 =
        Just
          ( Range
              { start = Position {line = 8, column = Just 13},
                end = Just (Position {line = 10, column = Just 13})
              }
          )
  let expectedRange3 =
        Just
          ( Range
              { start = Position {line = 9, column = Just 13},
                end = Just (Position {line = 11, column = Just 13})
              }
          )
  let expectedRange4 =
        Just
          ( Range
              { start = Position {line = 19, column = Just 13},
                end = Just (Position {line = 21, column = Just 13})
              }
          )
  let expectedRange5 =
        Just
          ( Range
              { start = Position {line = 20, column = Just 13},
                end = Just (Position {line = 22, column = Just 13})
              }
          )
  let expectedRange6 =
        Just
          ( Range
              { start = Position {line = 21, column = Just 13},
                end = Just (Position {line = 23, column = Just 13})
              }
          )
  let expectedRange7 =
        Just
          ( Range
              { start = Position {line = 32, column = Just 13},
                end = Just (Position {line = 34, column = Just 13})
              }
          )
  let expectedRange8 =
        Just
          ( Range
              { start = Position {line = 33, column = Just 13},
                end = Just (Position {line = 35, column = Just 13})
              }
          )
  let expectedRange9 =
        Just
          ( Range
              { start = Position {line = 43, column = Just 13},
                end = Just (Position {line = 45, column = Just 13})
              }
          )
  let expectedRange10 =
        Just
          ( Range
              { start = Position {line = 44, column = Just 13},
                end = Just (Position {line = 46, column = Just 13})
              }
          )
  let expectedRange11 =
        Just
          ( Range
              { start = Position {line = 55, column = Just 13},
                end = Just (Position {line = 57, column = Just 9})
              }
          )
  let expectedRanges =
        [ expectedRange1,
          expectedRange2,
          expectedRange3,
          expectedRange4,
          expectedRange5,
          expectedRange6,
          expectedRange7,
          expectedRange8,
          expectedRange9,
          expectedRange10,
          expectedRange11
        ]
  zipWithM_ (assertEqual "Check range") expectedRanges (map (range . location) diagnostic)
