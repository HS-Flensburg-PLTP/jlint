module DefaultComesLast.Tests where

import CheckResults
import DefaultComesLast (check)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAll :: IO ()
testAll = do
  testDefaultComesLast <- testDefaultComesLastIO
  runTestTT ("DefaultComesLast" ~: [testDefaultComesLast])
  return ()

testDefaultComesLastIO :: IO Test
testDefaultComesLastIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/DefaultComesLast/DefaultComesLast.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method test: Defaultcase in Switch-Case is not defined last" path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("DefaultComesLast" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.
