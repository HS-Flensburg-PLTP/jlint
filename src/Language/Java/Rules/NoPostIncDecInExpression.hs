module Language.Java.Rules.NoPostIncDecInExpression where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (Located (sourceSpan))
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let allowedExpressions = do
        statement <- universeBi cUnit
        checkStatement statement
  let allPostIncDec = do
        expression <- universeBi cUnit
        checkExpression expression
  postIncDec <- allPostIncDec
  if any (\exp -> eq IncludeSourceSpan exp postIncDec) allowedExpressions
    then mzero
    else createDiagnostic postIncDec path

checkStatement :: Stmt Parsed -> [Exp Parsed]
checkStatement (ExpStmt _ expression) = checkExpression expression
checkStatement (BasicFor _ _ _ expressions _) = concatMap checkExpression expressions
checkStatement _ = mzero

checkExpression :: Exp Parsed -> [Exp Parsed]
checkExpression exp@(PostIncrement _ _) = return exp
checkExpression exp@(PostDecrement _ _) = return exp
checkExpression _ = mzero

createDiagnostic :: Exp Parsed -> FilePath -> [RDF.Diagnostic]
createDiagnostic expr path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.NoPostIncDecInExpression"
        ("Die Verwendung von " ++ Markdown.code (showPostIncDec expr) ++ " ist an dieser Stelle nicht erlaubt, da es zu unleserlichem sowie unverständlichem Code führt.")
        (sourceSpan expr)
        path
    )

showPostIncDec :: Exp Parsed -> String
showPostIncDec (PostIncrement _ _) = "++"
showPostIncDec (PostDecrement _ _) = "--"
showPostIncDec _ = mzero
