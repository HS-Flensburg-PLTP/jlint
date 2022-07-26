module UnnecessaryVariables.Tests (testAllUnnecessaryVariables) where

import CheckResults
import RDF
import RessourceManager
import Test.HUnit
import UnnecessaryVariables (checkMethodVars)

testAllUnnecessaryVariables :: IO ()
testAllUnnecessaryVariables = do
  runTestTT (TestList [testUnnecessaryVariables])
  return ()

testUnnecessaryVariables =
  "Unnecessary Variables"
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/UnnecessaryVariables/UnnecessaryVariables.java"
          ( \(path, cUnit) -> do
              let diagResult = checkMethodVars cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Variable a is unnecessary"
          )
      ]
