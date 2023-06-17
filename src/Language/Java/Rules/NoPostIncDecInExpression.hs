module Language.Java.Rules.NoPostIncDecInExpression where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  statement <- universeBi cUnit
  checkStatement statement path

checkStatement :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkStatement (ExpStmt _ expression) path = checkExpression expression path
checkStatement (BasicFor _ _ _ (Just expressions) _) path = concatMap (`checkExpression` path) expressions
checkStatement _ _ = []

checkExpression :: Exp Parsed -> FilePath -> [RDF.Diagnostic]
checkExpression (PostIncrement sourceSpan exp) path =
  RDF.rangeDiagnostic
    "Language.Java.Rules.NoPostIncDecInExpression"
    "Post Increment ++ ist nicht in Variablenzuweisungen erlaubt."
    sourceSpan
    path :
  checkExpression exp path
checkExpression (PostDecrement sourceSpan exp) path =
  RDF.rangeDiagnostic
    "Language.Java.Rules.NoPostIncDecInExpression"
    "Post Decrement -- ist nicht in Variablenzuweisungen erlaubt."
    sourceSpan
    path :
  checkExpression exp path
checkExpression _ _ = []

{-
ExpStmt Exp
BasicFor (Maybe ForInit) (Maybe Exp) (Maybe [Exp]) Stmt

-}
