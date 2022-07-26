module EmptyLoopBody.Tests (testAllEmptyLoopBodies) where

import CheckResults
import EmptyLoopBody (check)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

-- data Test = TestCase Assertion
--           | TestList [Test]
--           | TestLabel String Test

{-
lamdafunction defines how a single testcase is structured. Each testcase can be defined using mutliple assertions (Assertion = IO ()).
If any of constituent assertions is executed and fails it terminates execution of the collective assertion / the testcase.
withCunit creates a testcase for each of the corresponding java - testcode - files. This Way it is possilbe to run the same
test for multiple inputs.
-}

testAllEmptyLoopBodies :: IO ()
testAllEmptyLoopBodies = do
  testEmptywhileLoop <- testEmptyWhileLoopIO
  testEmptyEnhancedForLoop <- testEmptyEnhancedForLoopIO
  testEmptyBasicForLoop <- testEmptyBasicForLoopIO
  testEmptyDoLoop <- testEmptyDoLoopIO
  runTestTT ("EmptyLoop" ~: [testEmptywhileLoop, testEmptyEnhancedForLoop, testEmptyBasicForLoop, testEmptyDoLoop])
  return ()

testEmptyWhileLoopIO :: IO Test
testEmptyWhileLoopIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/emptyLoopBody/EmptyWhileLoop.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = EmptyLoopBody.check cUnit path
                  checkMessage diagResults "Method testFunc: A While-Loop has a empty loop body." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("While" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.


testEmptyEnhancedForLoopIO :: IO Test
testEmptyEnhancedForLoopIO =
  do
    assertionList <-
      withCUnit
        "/test/emptyLoopBody/EmptyEnhancedForLoop.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = EmptyLoopBody.check cUnit path
                  checkMessage diagResults "Method testFunc: A ForEach-Lopp has a empty loop body." path
                  checkPath diagResults path 
              )
        )

    return ("ForEach" ~: test (map TestCase assertionList)) 

testEmptyBasicForLoopIO :: IO Test
testEmptyBasicForLoopIO =
  do
    assertionList <-
      withCUnit
        "/test/emptyLoopBody/EmptyBasicForLoop.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = EmptyLoopBody.check cUnit path
                  checkMessage diagResults "Method testFunc: A For-Loop has a empty loop body." path
                  checkPath diagResults path 
              )
        )

    return ("For" ~: test (map TestCase assertionList)) 

testEmptyDoLoopIO :: IO Test
testEmptyDoLoopIO =
  do
    assertionList <-
      withCUnit
        "/test/emptyLoopBody/EmptyDoLoop.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = EmptyLoopBody.check cUnit path
                  checkMessage diagResults "Method testFunc: A Do-Loop has a empty loop body." path
                  checkPath diagResults path 
              )
        )

    return ("Do" ~: test (map TestCase assertionList)) 