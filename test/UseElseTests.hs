module UseElseTests where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.UseElse (check)
import Language.Java.Syntax (CompilationUnit)
import RDF
import RessourceManager
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
  let file = "/test/UseElse.java"
   in test [file ~: withFile file useElse]

useElse :: CompilationUnit -> FilePath -> Assertion
useElse cUnit path = do
  let diagnostic = check cUnit path
  let expectedMsg1 = "Hier bitte ein `else` verwenden, damit sofort klar ist, dass der restliche Code nur ausgeführt wird, wenn die Bedingung nicht erfüllt ist."
  let expectedMsg2 = "Der `then`-Zweig der `if`- verlässt immer die Methode. Daher sollte nach der `if`-Anweisung keine weitere Anweisung folgen."
  assertEqual
    "Check message"
    [expectedMsg1, expectedMsg2]
    (map message diagnostic)
  -- This source span should be exclusive -> adapt language-java
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
              { start = Position {line = 12, column = Just 9},
                end = Just (Position {line = 18, column = Just 9})
              }
          )
  assertEqual
    "Check range"
    [expectedRange1, expectedRange2]
    (map (range . location) diagnostic)
