module NoNullPointerExceptionsForControlTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/NoNullPointerExceptionsForControl.java"
   in test [file ~: Tests.withParsedJavaFile file nullPointerExceptions]

nullPointerExceptions :: CompilationUnit -> FilePath -> Assertion
nullPointerExceptions cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 7, column = Just 13},
            end = Just (Position {line = 12, column = Just 9})
          }
  let expectedRange2 =
        Range
          { start = Position {line = 19, column = Just 13},
            end = Just (Position {line = 24, column = Just 9})
          }
  let expectedRanges =
        [ expectedRange1,
          expectedRange2
        ]
  let diagnostic = NoNullPointerExceptionsForControl.check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
