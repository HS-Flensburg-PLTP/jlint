module NoLoopBreak.Tests (testAllNoLoopBreaks) where

import CheckResults
import NoLoopBreak (check)
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

testAllNoLoopBreaks :: IO ()
testAllNoLoopBreaks = do
  testBreakLoop <- testBreakLoopIO
  testReturnLoop <- testReturnLoopIO

  runTestTT 
    ("EmptyLoop" 
      ~: 
        [ testBreakLoop,
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
        "/test/noLoopBreak/JavaFiles/BreakLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
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
        "/test/noLoopBreak/JavaFiles/ReturnLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with return" path
                  checkPath diagResults path 
              )
        )

    return ("Do-While-Loop" ~: test (map TestCase assertionList)) 
