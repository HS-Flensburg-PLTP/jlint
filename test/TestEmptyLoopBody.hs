
module TestEmptyLoopBody where

import EmptyLoopBody (check)
import RDF
import RessourceManager
import Test.HUnit

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

testEmptyLoopBodies :: IO ()
testEmptyLoopBodies = do
  runTestTT (TestList [testEmptyWhileLoop])
  return ()

testEmptyWhileLoop =
  "Empty While Loop" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit 
          "/test/EmptyLoopBody.java"
          ( \(path, cUnit) -> do
              let actual = EmptyLoopBody.check cUnit path
              actual @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
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
          { RDF.path = "/Users/lukasnolde/Documents/Informatik/Semester06/Projekt/Repositories/jlint",
            locationRange = Nothing
          },
      severity = Just (Left "WARNING"),
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
