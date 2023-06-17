module Language.Java.Rules.NoPostIncDecInExpression where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let statements = universeBi cUnit
  let allowedExpressions = concatMap checkStatement statements
  let allPostIncDec = universeBi cUnit
  concatMap (`createDiagnostic` path) (filter (\e -> not (any (comparePostIncDec e) allowedExpressions)) allPostIncDec)

comparePostIncDec :: Exp Parsed -> Exp Parsed -> Bool
comparePostIncDec (PostIncrement sp1 _) (PostIncrement sp2 _) = sp1 == sp2
comparePostIncDec (PostDecrement sp1 _) (PostDecrement sp2 _) = sp1 == sp2
comparePostIncDec _ _ = False

checkStatement :: Stmt Parsed -> [Exp Parsed]
checkStatement (ExpStmt _ expression) = checkExpression expression
checkStatement (BasicFor _ _ _ (Just expressions) _) = concatMap checkExpression expressions
checkStatement _ = []

checkExpression :: Exp Parsed -> [Exp Parsed]
checkExpression (PostIncrement sourceSpan exp) = [PostIncrement sourceSpan exp]
checkExpression (PostDecrement sourceSpan exp) = [PostDecrement sourceSpan exp]
checkExpression _ = []

createDiagnostic :: Exp Parsed -> FilePath -> [RDF.Diagnostic]
createDiagnostic (PostIncrement sourceSpan _) path =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.NoPostIncDecInExpression"
      ("Der Operator " ++ Markdown.code "++" ++ " ist an dieser Stelle nicht erlaubt, da es zu unleserlichem sowie unverständlichem Code führt.")
      sourceSpan
      path
  ]
createDiagnostic (PostDecrement sourceSpan _) path =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.NoPostIncDecInExpression"
      ("Der Operator " ++ Markdown.code "--" ++ " ist an dieser Stelle nicht erlaubt, da es zu unleserlichem sowie unverständlichem Code führt.")
      sourceSpan
      path
  ]
createDiagnostic _ _ = []
