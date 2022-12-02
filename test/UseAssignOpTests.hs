module UseAssignOpTests where

import Control.Monad (zipWithM_)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.UseAssignOp (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import System.Directory (getCurrentDirectory)
import Test.HUnit
import Tests (withParsedJavaFile)

tests :: Test
tests =
  let file = "/test/java/UseAssignOp.java"
   in test [file ~: Tests.withParsedJavaFile file useAssignOp]

useAssignOp :: CompilationUnit -> FilePath -> Assertion
useAssignOp cUnit path = do
  let expectedRange1 =
        Range
          { start = Position {line = 11, column = Just 17},
            end = Just (Position {line = 12, column = Just 17})
          }
  let expectedRanges =
        [ expectedRange1
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (range . location) diagnostic)
