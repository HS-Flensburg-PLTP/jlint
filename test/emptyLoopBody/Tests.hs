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

testAllEmptyLoopBodies :: IO ()
testAllEmptyLoopBodies = do
  testEmptywhileLoop <- testEmptyWhileLoopIO
  runTestTT testEmptywhileLoop
  return ()

{-
lamdafunction defines how a single testcase is structured. Each testcase can be defined using mutliple assertions (Assertion = IO ()).
If any of constituent assertions is executed and fails it terminates execution of the collective assertion / the testcase.
withCunit creates a testcase for each of the corresponding java - testcode - files. This Way it is possilbe to run the same
test for multiple inputs.
-}
testEmptyWhileLoopIO :: IO Test
testEmptyWhileLoopIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/emptyLoopBody/EmptyWhileLoop.java"
        ( return .
             map -- lamdafunction will be used to create a testcase for each testfile.
                ( \inputCode -> do
                    (cUnit, path) <- inputCode
                    let diagResults = EmptyLoopBody.check cUnit path
                    checkMessage diagResults "Method testFunc: A While-Loop has a empty loop body."
                    checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
                )
        )

    return ("Empty While Loop" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test. 
