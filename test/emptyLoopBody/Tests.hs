module EmptyLoopBody.Tests (testAllEmptyLoopBodies) where

import CheckResults
import EmptyLoopBody (check)
import RDF
import RessourceManager
import Test.HUnit

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

testAllEmptyLoopBodies :: IO ()
testAllEmptyLoopBodies = do
  runTestTT (TestList [testEmptyWhileLoop, testEmptyDoLoop, testEmptyBasicForLoop, testEmptEnhancedForLoop])
  return ()

testEmptyWhileLoop =
  "Empty While Loop" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/emptyLoopBody/EmptyWhileLoop.java"
          (\inputList -> map (\input -> do
              (path, cUnit) <- input
              let diagResults = EmptyLoopBody.check cUnit path
              checkPath diagResults path
              checkMessage diagResults "Method testFunc: A While-Loop has a empty loop body."
              -- diagResults @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )inputList)
      ]

testEmptyDoLoop =
  "Empty Do Loop"
    ~: test
      [ withCUnit
          "/test/emptyLoopBody/EmptyDoLoop.java"
          (\inputList -> map (\input -> do
              (path, cUnit) <- input
              let diagResults = EmptyLoopBody.check cUnit path
              checkPath diagResults path
              checkMessage diagResults "Method testFunc: A Do-Loop has a empty loop body."
          )inputList)
      ]

testEmptyBasicForLoop =
  "Empty Basic For Loop"
    ~: test
      [ withCUnit
          "/test/emptyLoopBody/EmptyBasicForLoop.java"
          (\inputList -> map (\input -> do
              (path, cUnit) <- input
              let diagResults = EmptyLoopBody.check cUnit path
              checkPath diagResults path
              checkMessage diagResults "Method testFunc: A For-Loop has a empty loop body."
          ) inputList)
      ]

testEmptEnhancedForLoop =
  "Empty ForEach Loop"
    ~: test
      [ withCUnit
          "/test/emptyLoopBody/EmptyEnhancedForLoop.java"
          (\inputList -> map (\input -> do
              (path, cUnit) <- input
              let diagResults = EmptyLoopBody.check cUnit path
              checkPath diagResults path
              checkMessage diagResults "Method testFunc: A ForEach-Lopp has a empty loop body."
          )inputList)
      ]

{-testEmptyWhileLoop =
  test -- test can be called on List of testables
    [ "Empty While Loop"
        ~: withCUnit -- ~: adds label and applies assertion to Testcase (Testcase . assert) -> this is a testcase
          "/test/EmptyLoopBody.java"
          ( \(path, cUnit) -> do
              let actual = EmptyLoopBody.check cUnit path
              actual @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
    ]
-}
expected :: Diagnostic
expected =
  Diagnostic
    { message = "Method testFunc: A While-Loop has a empty loop body.",
      location =
        Location
          { RDF.path = "/Users/lukasnolde/Documents/Informatik/Semester06/Projekt/Repositories/jlint/test/emptyLoopBody/EmptyLoopBody.java",
            locationRange = Nothing
          },
      severity = Warning,
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }

{- TODO:
    1. Find Structure for test: e.g: Folder for each test-, input-, expected- file
    2. Suggest: using balckboxtest, define classes of equivalence ()
        ,define Unit test as well as Integrationtests(to check interaction between parser and jlint)
    3. Convert RDF.path to work on every device
-}
