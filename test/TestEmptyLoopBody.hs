module TestEmptyLoopBody where
import Test.HUnit
import Language.Java.Parser (compilationUnit, parser)
import EmptyLoopBody(check)
import Language.Java.Syntax

parseJava :: String -> Maybe Language.Java.Syntax.CompilationUnit
parseJava input = do
    let result  = parser compilationUnit input
    case result of 
        Left _ -> Nothing
        Right cUnit -> Just cUnit


testEmptyDoLoop = TestCase( do 
    cUnit <- parseJava "while(++i < --j);"
    let (x : xs) = EmptyLoopBody.check cUnit ""
    print x
    assert(return ""))
    -- assertEqual "tesmsg" "A While-Loop has a empty loop body." x.message )
