
module TestEmptyLoopBody where 
import Test.HUnit
import EmptyLoopBody(check)
import RessourceManager

-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

testEmptyDoLoop :: IO()
testEmptyDoLoop = do
    runTestTT testEmptyDoLoob
    return ()

testEmptyDoLoob = TestList [
    "Empty Do Loop" ~: withCUnit "/test/Strings2.java" (\(path, cUnit) -> do
            let actual = EmptyLoopBody.check cUnit path
            let expected = EmptyLoopBody.check cUnit path
            False @? "Empty do Loop does not match"
        )
    ]
