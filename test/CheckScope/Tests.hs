module CheckScope.Tests where

import CheckResults
import CheckScope (check)
import RDF
import RessourceManager
import Test.HUnit

testAll :: IO ()
testAll = do
  runTestTT (TestList [ifElseTest, ifThenElseTest, enhancedForTest, doTest, forTest, whileTest])
  return ()

ifElseTest =
  "Var belong in if-else scope"
    ~: test
      [ withCUnit
          "/test/checkScope/IfThenScope.java"
          ( \(path, cUnit) -> do
              let diagResult = check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: The scope of variable i can be reduced."
          )
      ]

ifThenElseTest =
  "Var i belong in if-then-else scope"
    ~: test
      [ withCUnit
          "/test/checkScope/IfThenElseScope.java"
          ( \(path, cUnit) -> do
              let diagResult = check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: The scope of variable i can be reduced."
          )
      ]

enhancedForTest =
  "Var i belong in EnhancedFor scope"
    ~: test
      [ withCUnit
          "/test/checkScope/EnhancedForScope.java"
          ( \(path, cUnit) -> do
              let diagResult = check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: The scope of variable i can be reduced."
          )
      ]

doTest =
  "Var i belong in Do scope"
    ~: test
      [ withCUnit
          "/test/checkScope/DoScope.java"
          ( \(path, cUnit) -> do
              let diagResult = check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: The scope of variable i can be reduced."
          )
      ]

forTest =
  "Var i belong in For scope"
    ~: test
      [ withCUnit
          "/test/checkScope/ForScope.java"
          ( \(path, cUnit) -> do
              let diagResult = check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: The scope of variable i can be reduced."
          )
      ]

whileTest =
  "Var i belong in While scope"
    ~: test
      [ withCUnit
          "/test/checkScope/WhileScope.java"
          ( \(path, cUnit) -> do
              let diagResult = check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: The scope of variable i can be reduced."
          )
      ]
