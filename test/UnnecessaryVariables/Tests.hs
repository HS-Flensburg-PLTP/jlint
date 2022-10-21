module UnnecessaryVariables.Tests (testAllUnnecessaryVariables) where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.UnnecessaryVariables (checkMethodVars)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAllUnnecessaryVariables :: IO ()
testAllUnnecessaryVariables = do
  testUnnecessaryVariables <- testUnnecessaryVariablesIO
  runTestTT
    ( "Unnecessary Variables"
        ~: [testUnnecessaryVariables]
    )
  return ()

testUnnecessaryVariablesIO :: IO Test
testUnnecessaryVariablesIO =
  do
    assertionList <-
      withCUnit
        "/test/UnnecessaryVariables/UnnecessaryVariables.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkMethodVars cUnit path
                  checkMessage diagResults "Method testFunc: Variable a is unnecessary" path
                  checkPath diagResults path
              )
        )

    return ("UnnecessaryVariables" ~: test (map TestCase assertionList))
