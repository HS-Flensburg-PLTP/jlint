module Language.Java.Rules.NoIncDecInExpression (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (Located (sourceSpan))
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let allowedIncDec = filter isIncDec (universeBi cUnit >>= loopUpdateExpressions)
  incDec <- filter isIncDec (universeBi cUnit)
  if any (eq IncludeSourceSpan incDec) allowedIncDec
    then mzero
    else return (createDiagnostic incDec path)
  where
    loopUpdateExpressions :: Stmt Parsed -> [Exp Parsed]
    loopUpdateExpressions (ExpStmt _ expression) = return expression
    loopUpdateExpressions (BasicFor _ _ _ expressions _) = expressions
    loopUpdateExpressions _ = mzero

createDiagnostic :: Exp Parsed -> FilePath -> RDF.Diagnostic
createDiagnostic expr =
  RDF.rangeDiagnostic
    "Language.Java.Rules.NoIncDecInExpression"
    [ "Die Verwendung von",
      Markdown.code (prettyPrint expr),
      "in Ausdrücken ist nicht erlaubt, da sie zu unleserlichem sowie unverständlichem Code führt.",
      "Erlaubt ist es als Fortsetzung in einer",
      Markdown.code "for" ++ "-Schleife",
      "oder als Anweisung als Kurzform von",
      Markdown.code "i = i + 1;" ++ "."
    ]
    (sourceSpan expr)

isIncDec :: Exp p -> Bool
isIncDec (PostIncrement _ _) = True
isIncDec (PostDecrement _ _) = True
isIncDec (PreIncrement _ _) = True
isIncDec (PreDecrement _ _) = True
isIncDec _ = False
