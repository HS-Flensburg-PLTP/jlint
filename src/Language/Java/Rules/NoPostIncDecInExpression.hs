module Language.Java.Rules.NoPostIncDecInExpression where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let statements = universeBi cUnit
  let allowedExpressions = do
        statement <- statements
        checkStatement statement
  let allExpressions = universeBi cUnit
  let allPostIncDec = do
        expression <- allExpressions
        checkExpression expression
  postIncDec <- allPostIncDec
  if any (\(_, allowedExpSourceSpan) -> allowedExpSourceSpan == snd postIncDec) allowedExpressions
    then []
    else createDiagnostic postIncDec path

checkStatement :: Stmt Parsed -> [(String, SourceSpan)]
checkStatement (ExpStmt _ expression) = checkExpression expression
checkStatement (BasicFor _ _ _ expressions _) = concatMap checkExpression expressions
checkStatement _ = mzero

checkExpression :: Exp Parsed -> [(String, SourceSpan)]
checkExpression (PostIncrement sourceSpan _) = return ("++", sourceSpan)
checkExpression (PostDecrement sourceSpan _) = return ("--", sourceSpan)
checkExpression _ = mzero

createDiagnostic :: (String, SourceSpan) -> FilePath -> [RDF.Diagnostic]
createDiagnostic (expr, sourceSpan) path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.NoPostIncDecInExpression"
        ("Die Verwendung von " ++ Markdown.code expr ++ " ist an dieser Stelle nicht erlaubt, da es zu unleserlichem sowie unverständlichem Code führt.")
        sourceSpan
        path
    )
