module Language.Java.Rules.ConstantPropagation (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (IfThen _ (BinOp otherExp Equal (Lit lit)) thenStmt) = do
      test <- universeBi thenStmt
      case test of
        (MethodCall _ [innerExp]) ->
          if innerExp == otherExp
            then
              [ RDF.rangeDiagnostic
                  "Language.Java.Rules.ConstantPropagation"
                  "ConstantPropagation"
                  dummySourceSpan
                  path
              ]
            else mzero
        _ -> mzero
    checkStmt (IfThen _ (BinOp (Lit lit) Equal otherExp) thenStmt) =
      mzero
    checkStmt (IfThenElse _ (BinOp otherExp Equal (Lit lit)) thenStmt elseStmt) =
      mzero
    checkStmt (IfThenElse _ (BinOp (Lit lit) Equal otherExp) thenStmt elseStmt) =
      mzero
    checkStmt _ = mzero
