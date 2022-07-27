module NoLoopBreak.Tests (testAllEmptyLoopBodies) where

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
  testBreakDoWhileLoop <- testBreakDoWhileLoopIO
  testBreakEnhancedForLoop <- testBreakEnhancedForLoopIO
  testBreakForLoop <- testBreakForLoopIO
  testBreakNestedForLoop <- testBreakNestedForLoopIO
  testBreakNestedWhileLoop <- testBreakNestedWhileLoopIO
  testBreakWhileLoop <- testBreakWhileLoopIO
  testReturnDoWhileLoop <- testReturnDoWhileLoopIO
  testReturnEnhancedForLoop <- testReturnEnhancedForLoopIO
  testReturnForLoop <- testReturnForLoopIO
  testReturnNestedForLoop <- testReturnNestedForLoopIO
  testReturnNestedWhileLoop <- testReturnNestedWhileLoopIO
  testReturnWhileLoop <- testReturnWhileLoopOP
  
  runTestTT 
    ("EmptyLoop" 
      ~: 
        [ testBreakDoWhileLoop,
          testBreakEnhancedForLoop,
          testBreakForLoop,
          testBreakNestedForLoop,
          testBreakNestedWhileLoop,
          testBreakWhileLoop,
          testReturnDoWhileLoop,
          testReturnEnhancedForLoop,
          testReturnForLoop,
          testReturnNestedForLoop,
          testReturnNestedWhileLoop,
          testReturnWhileLoop
        ]
    )
  return ()

testAllNoLoopBreaksIO :: IO Test
testAllNoLoopBreaksIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/BreakDoWhileLoop.java"
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

testBreakEnhancedForLoopIO :: IO Test
testBreakEnhancedForLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/BreakEnhancedForLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("Enhanced-For-Loop" ~: test (map TestCase assertionList)) 

testBreakForLoopIO :: IO Test
testBreakForLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/BreakForLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("For-Loop" ~: test (map TestCase assertionList)) 

testBreakNestedForLoopIO :: IO Test
testBreakNestedForLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/BreakNestedForLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("Nested-For-Loop" ~: test (map TestCase assertionList)) 



testBreakNestedWhileLoopIO :: IO Test
testBreakNestedWhileLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/BreakNestedWhileLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("Nested-While-Loop" ~: test (map TestCase assertionList)) 

testBreakWhileLoopIO :: IO Test
testBreakWhileLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/BreakWhileLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("While-Loop" ~: test (map TestCase assertionList)) 



-- With return

testReturnDoWhileLoopIO :: IO Test
testReturnDoWhileLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/ReturnDoWhileLoop.java"
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

testReturnEnhancedForLoopIO :: IO Test
testReturnEnhancedForLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/ReturnEnhancedForLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("Enhanced-For-Loop" ~: test (map TestCase assertionList)) 

testReturnForLoopIO :: IO Test
testReturnForLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/ReturnForLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("For-Loop" ~: test (map TestCase assertionList)) 

testReturnNestedForLoopIO :: IO Test
testReturnNestedForLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/ReturnNestedForLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("Nested-For-Loop" ~: test (map TestCase assertionList)) 

testReturnNestedWhileLoopIO :: IO Test
testReturnNestedWhileLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/ReturnNestedWhileLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("Nested-While-Loop" ~: test (map TestCase assertionList))

testReturnWhileLoopIO :: IO Test
testReturnWhileLoopIO =
  do
    assertionList <-
      withCUnit 
        "/test/noLoopBreak/JavaFiles/ReturnWhileLoop.java"
        ( return
            . map 
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = NoLoopBreak.check cUnit path
                  checkMessage diagResults "Method testFunc: Exit Loop with break" path
                  checkPath diagResults path 
              )
        )

    return ("While-Loop" ~: test (map TestCase assertionList))
