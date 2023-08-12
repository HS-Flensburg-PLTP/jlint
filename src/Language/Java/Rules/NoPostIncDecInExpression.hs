module Language.Java.Rules.NoPostIncDecInExpression where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (Located (sourceSpan))
import Language.Java.Syntax
import qualified Language.Java.Syntax.Exp as Exp
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let allowedPostIncDec = do
        statement <- universeBi cUnit
        checkStatement statement
  postIncDec <- filter Exp.isPostIncDec (universeBi cUnit)
  if any (eq IncludeSourceSpan postIncDec) allowedPostIncDec
    then mzero
    else createDiagnostic postIncDec path

checkStatement :: Stmt Parsed -> [Exp Parsed]
checkStatement (ExpStmt _ expression) =
  if Exp.isPostIncDec expression
    then return expression
    else []
checkStatement (BasicFor _ _ _ expressions _) = filter Exp.isPostIncDec expressions
checkStatement _ = mzero

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
