module Language.Java.Rules.NoPostIncDecInExpression (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (Located (sourceSpan))
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let allowedPostIncDec = filter isPostIncDec (universeBi cUnit >>= loopUpdateExpressions)
  postIncDec <- filter isPostIncDec (universeBi cUnit)
  if any (eq IncludeSourceSpan postIncDec) allowedPostIncDec
    then mzero
    else return (createDiagnostic postIncDec path)
  where
    loopUpdateExpressions :: Stmt Parsed -> [Exp Parsed]
    loopUpdateExpressions (ExpStmt _ expression) = return expression
    loopUpdateExpressions (BasicFor _ _ _ expressions _) = expressions
    loopUpdateExpressions _ = mzero

createDiagnostic :: Exp Parsed -> FilePath -> RDF.Diagnostic
createDiagnostic expr =
  RDF.rangeDiagnostic
    "Language.Java.Rules.NoPostIncDecInExpression"
    [ "Die Verwendung von",
      Markdown.code (prettyPrint expr),
      "ist so nicht erlaubt, da es zu unleserlichem sowie unverständlichem Code führt.",
      "Erlaubt ist es als Fortsetzung in einer",
      Markdown.code "for" ++ "-Schleife",
      "oder als Anweisung als Kurzform von",
      Markdown.code "i = i + 1;" ++ "."
    ]
    (sourceSpan expr)

isPostIncDec :: Exp p -> Bool
isPostIncDec (PostIncrement _ _) = True
isPostIncDec (PostDecrement _ _) = True
isPostIncDec _ = False
