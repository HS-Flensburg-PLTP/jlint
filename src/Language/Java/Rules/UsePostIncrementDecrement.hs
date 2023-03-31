module Language.Java.Rules.UsePostIncrementDecrement where

import qualified RDF
import Language.Java.Syntax
import Data.Generics.Uniplate.Data (universeBi)
import Control.Monad


check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  expr <- universeBi cUnit
  checkExpr expr
  where
    checkExpr (PreIncrement _) =
      return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrement" "Anstelle des PreIncrement Operators ++x sollte hier der PostIncrement Operator x++ verwendet werden." dummySourceSpan path)
    checkExpr (PreDecrement _) = 
      return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrement" "Anstelle des PreDecrement Operators --x sollte hier der PostDecrement Operator x-- verwendet werden." dummySourceSpan path)
    checkExpr _ = mzero
