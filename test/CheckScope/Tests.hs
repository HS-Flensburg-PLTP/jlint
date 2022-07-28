module CheckScope.Tests where

import CheckResults
import CheckScope (check)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAll :: IO ()
testAll = do
  testIfThen <- testIfThenIO
  testIfThenElse <- testIfThenElseIO
  testEnhancedFor <- testEnhancedForIO
  testDo <- testDoIO
  testFor <- testFor
  testWhile <- testWhileIO
  runTestTT ("SimplifyBooleanReturn" ~: [testIfThen, testIfThenElse, testEnhancedFor, testDo, testFor, testWhile])
  return ()

testIfThenIO :: IO Test
testIfThenIO =
  do
    assertionList <-
      withCUnit
        "/test/checkScope/IfThenScope.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: The scope of variable i can be reduced." path
                  checkPath diagResults path
              )
        )

    return ("IfThen" ~: test (map TestCase assertionList))

testIfThenElseIO :: IO Test
testIfThenElseIO =
  do
    assertionList <-
      withCUnit
        "/test/checkScope/IfThenElseScope.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: The scope of variable i can be reduced." path
                  checkPath diagResults path
              )
        )

    return ("IfThen" ~: test (map TestCase assertionList))

testEnhancedForIO :: IO Test
testEnhancedForIO =
  do
    assertionList <-
      withCUnit
        "/test/checkScope/EnhancedForScope.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: The scope of variable i can be reduced." path
                  checkPath diagResults path
              )
        )

    return ("IfThenElse" ~: test (map TestCase assertionList))

testDoIO :: IO Test
testDoIO =
  do
    assertionList <-
      withCUnit
        "/test/checkScope/DoScope.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: The scope of variable i can be reduced." path
                  checkPath diagResults path
              )
        )

    return ("Do" ~: test (map TestCase assertionList))

testForIO :: IO Test
testForIO =
  do
    assertionList <-
      withCUnit
        "/test/checkScope/ForScope.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: The scope of variable i can be reduced." path
                  checkPath diagResults path
              )
        )

    return ("For" ~: test (map TestCase assertionList))

testWhileIO :: IO Test
testWhileIO =
  do
    assertionList <-
      withCUnit
        "/test/checkScope/WhileScope.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: The scope of variable i can be reduced." path
                  checkPath diagResults path
              )
        )

    return ("While" ~: test (map TestCase assertionList))
