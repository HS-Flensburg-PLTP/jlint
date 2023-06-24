module Language.Java.Rules.NoPostIncDecInExpression where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let statements = universeBi cUnit
  let allowedExpressions = concatMap checkStatement statements
  let allExpressions = universeBi cUnit
  let allPostIncDec = concatMap checkExpression allExpressions
  postIncDec <- allPostIncDec
  if any (\allowedExp -> sourceSpan allowedExp == sourceSpan postIncDec) allowedExpressions
    then []
    else createDiagnostic postIncDec path

checkStatement :: Stmt Parsed -> [Exp Parsed]
checkStatement (ExpStmt _ expression) = checkExpression expression
checkStatement (BasicFor _ _ _ (Just expressions) _) = concatMap checkExpression expressions
checkStatement _ = mzero

checkExpression :: Exp Parsed -> [Exp Parsed]
checkExpression expr@(PostIncrement _ _) = return expr
checkExpression expr@(PostDecrement _ _) = return expr
checkExpression _ = mzero

createDiagnostic :: Exp Parsed -> FilePath -> [RDF.Diagnostic]
createDiagnostic (PostIncrement sourceSpan _) path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.NoPostIncDecInExpression"
        ("Der Operator " ++ Markdown.code "++" ++ " ist an dieser Stelle nicht erlaubt, da es zu unleserlichem sowie unverständlichem Code führt.")
        sourceSpan
        path
    )
createDiagnostic (PostDecrement sourceSpan _) path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.NoPostIncDecInExpression"
        ("Der Operator " ++ Markdown.code "--" ++ " ist an dieser Stelle nicht erlaubt, da es zu unleserlichem sowie unverständlichem Code führt.")
        sourceSpan
        path
    )
createDiagnostic _ _ = mzero
