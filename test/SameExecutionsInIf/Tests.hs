module SameExecutionsInIf.Tests where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import RDF
import RessourceManager
import SameExecutionsInIf (check)
import Test.HUnit

testAll :: IO ()
testAll = do
  testAllSame <- testAllSameIO
  testThreeSame <- testThreeSameIO
  runTestTT ("SameExecutionsInIf" ~: [testAllSame, testThreeSame])
  return ()

testAllSameIO :: IO Test
testAllSameIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/SameExecutionsInIf/AllSame.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: In an if-then-else statement, all lines of code are the same and can be swapped out." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("AllSame" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testThreeSameIO :: IO Test
testThreeSameIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/SameExecutionsInIf/ThreeSame.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: In an if-then-else statement, 3 line(s) of code is/are the same and can be swapped out." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("ThreeSame" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.
