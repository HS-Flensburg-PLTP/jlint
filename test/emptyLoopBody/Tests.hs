module EmptyLoopBody.Tests (testAllEmptyLoopBodies) where

import CheckResults
import EmptyLoopBody (check)
import RDF
import RessourceManager
import Test.HUnit
import Language.Java.Syntax
import Language.Java.Parser (compilationUnit, parser)

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

testAllEmptyLoopBodies :: IO ()
testAllEmptyLoopBodies = do
  emptyWhileLoopResults <- testEmptyWhileLoopIO
  let resutls = "Empty While Loop" ~: emptyWhileLoopResults

  runTestTT (TestList [resutls])
  return ()

whileTest :: [IO (CompilationUnit, FilePath)] -> IO [IO ()]
whileTest testInputList=
  return (map mapfunc testInputList)
  where
    mapfunc :: IO (CompilationUnit, FilePath) -> IO ()
    mapfunc testInput = do
        (cUnit, path) <- testInput
        let diagResults = EmptyLoopBody.check cUnit path
        checkPath diagResults path
        checkMessage diagResults "Method testFunc: A While-Loop has a empty loop body."


testEmptyWhileLoopIO =
  withCUnit "/test/emptyLoopBody/EmptyWhileLoop.java" whileTest


