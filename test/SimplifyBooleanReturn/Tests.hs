module SimplifyBooleanReturn.Tests where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.SimplifyBooleanReturn (check)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAll :: IO ()
testAll = do
  testIfThen <- testIfThenIO
  testIfThenElse <- testIfThenElseIO
  runTestTT ("SimplifyBooleanReturn" ~: [testIfThen, testIfThenElse])
  return ()

testIfThenIO :: IO Test
testIfThenIO =
  do
    assertionList <-
      withCUnit
        "/test/SimplifyBooleanReturn/IfThen.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A if-then part with literal return can be simplified." path
                  checkPath diagResults path
              )
        )

    return ("IfThen" ~: test (map TestCase assertionList))

testIfThenElseIO :: IO Test
testIfThenElseIO =
  do
    assertionList <-
      withCUnit
        "/test/SimplifyBooleanReturn/IfThenElse.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: A if-then-else part with literal return can be simplified." path
                  checkPath diagResults path
              )
        )

    return ("IfThen" ~: test (map TestCase assertionList))
