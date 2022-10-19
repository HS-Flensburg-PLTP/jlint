module NeedBraces.Tests where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.NeedBraces (check)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAll :: IO ()
testAll = do
  testDo <- testDoIO
  testWhile <- testWhileIO
  testBasicFor <- testBasicForIO
  testIfThen <- testIfThenIO
  testIfThenElse <- testIfThenElseIO
  testEnhancedFor <- testEnhancedForIO
  runTestTT ("NoBraces" ~: [testDo, testWhile, testBasicFor, testIfThen, testIfThenElse, testEnhancedFor])
  return ()

testDoIO :: IO Test
testDoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NeedBraces/NoDoBraces.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A Do-Part contains no braces." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("Do" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testWhileIO :: IO Test
testWhileIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NeedBraces/NoWhileBraces.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A While-Part contains no braces." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("While" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testBasicForIO :: IO Test
testBasicForIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NeedBraces/NoBasicForBraces.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A For-Part contains no braces." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("BasicFor" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testIfThenIO :: IO Test
testIfThenIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NeedBraces/NoIfThenBraces.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A IfThen-Part contains no braces." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("IfThen" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testIfThenElseIO :: IO Test
testIfThenElseIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NeedBraces/NoIfThenElseBraces.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A IfThenElse-Part contains no braces." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("IfThenElse" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testEnhancedForIO :: IO Test
testEnhancedForIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NeedBraces/NoEnhancedForBraces.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A ForEach-Part contains no braces." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("EnhancedFor" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.
