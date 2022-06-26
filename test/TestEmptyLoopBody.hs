module TestEmptyLoopBody where 
import Test.HUnit
import Language.Java.Parser (compilationUnit, parser)
import EmptyLoopBody(check)
import Language.Java.Syntax
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import UnliftIO.Exception
import System.Directory

before :: IO CompilationUnit
before = do
    path <- getCurrentDirectory 
    parseJava (path ++ "/test/Strings2.java")


parseJava :: String -> IO CompilationUnit
parseJava path = do
    input <- readFile path
    let result  = parser compilationUnit input
    case result of 
        Left error -> throwString (show error)
        Right cUnit -> return cUnit

testEmptyDoLoop :: IO()
testEmptyDoLoop = do
    cUnit <- before 
    print cUnit

-- testEmptyDoLoop = TestCase( do 
    -- cUnit <- parseJava "while(++i < --j);"
    -- let (x : xs) = EmptyLoopBody.check cUnit ""
    -- print x
    -- assert(return ""))
    -- assertEqual "tesmsg" "A While-Loop has a empty loop body." x.message )
