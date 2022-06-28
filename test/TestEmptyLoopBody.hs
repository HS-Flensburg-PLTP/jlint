module TestEmptyLoopBody where
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax

parseJava :: String -> Maybe Language.Java.Syntax.CompilationUnit
parseJava input = do
    let result  = parser compilationUnit input
    case result of 
        Left _ -> Nothing
        Right cUnit -> Just cUnit


