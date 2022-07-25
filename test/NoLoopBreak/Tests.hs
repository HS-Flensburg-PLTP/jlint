module NoLoopBreak.Tests (testAllNoLoopBreaks) where

import CheckResults
import NoLoopBreak
import RDF
import RessourceManager
import Test.HUnit

testAllNoLoopBreaks :: IO ()
testAllNoLoopBreaks = do
  runTestTT
    ( TestList
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

testBreakDoWhileLoop =
  "Break Do-While-Loop with Break" ~: 
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/BreakDoWhileLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with break"
          )
      ]

testBreakEnhancedForLoop =
  "Break Enhanced-For-Loop with Break" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/BreakEnhancedForLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with break"
          )
      ]

testBreakForLoop =
  "Break For-Loop with Break" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/BreakForLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with break"
          )
      ]

testBreakNestedForLoop =
  "Break Nested-For-Loop with Break" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/BreakNestedForLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with break"
          )
      ]

testBreakNestedWhileLoop =
  "Break Nested-While-Loop with Break" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/BreakNestedWhileLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with break"
          )
      ]

testBreakWhileLoop =
  "Break While-Loop with Break" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/BreakWhileLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with break"
          )
      ]

testReturnDoWhileLoop =
  "Break Do-While-Loop with Return" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/ReturnDoWhileLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with return"
          )
      ]

testReturnEnhancedForLoop =
  "Break Enhanced-For-Loop with Return" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/ReturnEnhancedForLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with return"
          )
      ]

testReturnForLoop =
  "Break For-Loop with Return" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/ReturnForLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with return"
          )
      ]

testReturnNestedForLoop =
  "Break Nested-For-Loop with Return" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/ReturnNestedForLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with return"
          )
      ]

testReturnNestedWhileLoop =
  "Break Nested-While-Loop with Return" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/ReturnNestedWhileLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with return"
          )
      ]

testReturnWhileLoop =
  "Break While-Loop with Return" ~:
    test
      [ withCUnit
          "/test/noLoopBreak/JavaFiles/ReturnWhileLoop.java"
          ( \(path, cUnit) -> do
              let diagResult = NoLoopBreak.check cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method testFunc: Exit Loop with return"
          )
      ]
