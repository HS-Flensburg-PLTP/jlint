module Language.Java.Rules.NoPostIncDecInExpression where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  varInit <- universeBi cUnit
  checkVarInits varInit path

checkVarInits :: VarInit -> FilePath -> [RDF.Diagnostic]
checkVarInits (InitExp exp) path = checkExpression exp path
checkVarInits _ _ = []

checkExpression :: Exp -> FilePath -> [RDF.Diagnostic]
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
