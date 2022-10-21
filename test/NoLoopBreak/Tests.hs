module NoLoopBreak.Tests (testAllNoLoopBreaks) where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.NoLoopBreak (check)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAllNoLoopBreaks :: IO ()
testAllNoLoopBreaks = do
  testBreakLoop <- testBreakLoopIO
  testReturnLoop <- testReturnLoopIO

  runTestTT
    ( "EmptyLoop"
        ~: [ testBreakLoop,
             testReturnLoop
           ]
    )
  return ()

-- Break Loop using break

testBreakLoopIO :: IO Test
testBreakLoopIO =
  do
    assertionList <-
      withCUnit
        "/test/NoLoopBreak/JavaFiles/BreakLoop.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path
              )
        )

    return ("Do-While-Loop" ~: test (map TestCase assertionList))

-- Break Loop using return

testReturnLoopIO :: IO Test
testReturnLoopIO =
  do
    assertionList <-
      withCUnit
        "/test/NoLoopBreak/JavaFiles/ReturnLoop.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with return" path
                  checkPath diagResults path
              )
        )

    return ("Do-While-Loop" ~: test (map TestCase assertionList))
