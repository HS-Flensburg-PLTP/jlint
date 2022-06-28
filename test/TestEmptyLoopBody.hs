module TestEmptyLoopBody where

import EmptyLoopBody (check)
import RDF
import RessourceManager
import Test.HUnit

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

testEmptyLoopBodies :: IO ()
testEmptyLoopBodies = do
  runTestTT testEmptyDoLoop
  return ()

testEmptyDoLoop =
  TestList
    [ "Empty Do Loop"
        ~: withCUnit
          "/test/EmptyLoopBody.java"
          ( \(path, cUnit) -> do
              let actual = EmptyLoopBody.check cUnit path
              print actual
              actual @=? [expected]
          )
    ]

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
-}