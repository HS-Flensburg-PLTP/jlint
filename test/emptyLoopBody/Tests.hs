module EmptyLoopBody.Tests (testAllEmptyLoopBodies) where

import CheckResults
import EmptyLoopBody (check)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

--------------

testAllEmptyLoopBodies :: IO ()
testAllEmptyLoopBodies = do
  emptyWhileLoopResults <- testEmptyWhileLoopIO
  let resutls = "Empty While Loop" ~: emptyWhileLoopResults

  runTestTT (TestList [resutls])
  return ()

------------
testEmptyWhileLoopIO :: IO [IO ()]
testEmptyWhileLoopIO =
  withCUnit
    "/test/emptyLoopBody/EmptyWhileLoop.java"
    ( return .
           map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = EmptyLoopBody.check cUnit path
                  checkPath diagResults path
                  checkMessage diagResults "Method testFunc: A While-Loop has a empty loop body."
              )
    )

-- OLD
-- testAllEmptyLoopBodies :: IO ()
-- testAllEmptyLoopBodies = do
--   runTestTT (TestList [testEmptyWhileLoop, testEmptyDoLoop, testEmptyBasicForLoop, testEmptEnhancedForLoop])
--   return ()

-- testEmptyWhileLoop =
--   "Empty While Loop" -- ~: adds label to Test (Testlist)
--     ~: test -- test can be called on List of testables
--       withCUnit
--       "/test/emptyLoopBody/EmptyWhileLoop.java"
--       map
--       ( \input -> do
--           (path, cUnit) <- input
--           let diagResults = EmptyLoopBody.check cUnit path
--           checkPath diagResults path
--           checkMessage diagResults "Method testFunc: A While-Loop has a empty loop body."
--           -- diagResults @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
--       )
