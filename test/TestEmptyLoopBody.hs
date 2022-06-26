
module TestEmptyLoopBody where 
import Test.HUnit
import Language.Java.Parser (compilationUnit, parser)
import EmptyLoopBody(check)
import Language.Java.Syntax
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import UnliftIO.Exception
import System.Directory
import Control.Monad.Reader ( runReader, MonadReader(ask), Reader )


before :: FilePath -> IO (FilePath, CompilationUnit)
before pathFromRoot = do
    path <- getCurrentDirectory 
    cUnit<- parseJava (path ++ pathFromRoot)
    return (path, cUnit)


parseJava :: String -> IO CompilationUnit
parseJava path = do
    input <- readFile path
    let result  = parser compilationUnit input
    case result of 
        Left error -> throwString (show error)
        Right cUnit -> return cUnit

after :: (FilePath, CompilationUnit) -> IO ()
after (fPath, cUnit) =
    -- implement teardown here
    return ()

withCUnit :: String -> ((FilePath, CompilationUnit) -> IO ()) -> IO ()
withCUnit fPath =
    bracket ((before fPath)) after -- bracket before after during

testEmptyDoLoop :: IO()
testEmptyDoLoop = do
    runTestTT test1
    return ()
    -- cUnit <- before 
    -- print cUnit

test1 = TestList [
    "Empty Do Loop" ~: withCUnit "/test/Strings2.java" (\(path, cUnit) -> do
            let actual = EmptyLoopBody.check cUnit path
            let expected = EmptyLoopBody.check cUnit path
            False @? "Empty do Loop does not match"
        )
    ]


-- testEmptyDoLoop = TestCase( do 
    -- cUnit <- parseJava "while(++i < --j);"
    -- let (x : xs) = EmptyLoopBody.check cUnit ""
    -- print x
    -- assert(return ""))
    -- assertEqual "tesmsg" "A While-Loop has a empty loop body." x.message )
