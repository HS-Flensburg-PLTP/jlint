module Language.Java.Rules.NeedBraces (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Stmt as Stmt
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt path

checkStmt :: Stmt -> FilePath -> [RDF.Diagnostic]
checkStmt (IfThen _ _ (StmtBlock _)) _ = mzero
checkStmt (IfThen _ _ stmt) path = return (diagnostic stmt path)
checkStmt (IfThenElse _ _ (StmtBlock _) (StmtBlock _)) _ = mzero
checkStmt (IfThenElse _ _ stmt (StmtBlock _)) path = return (diagnostic stmt path)
checkStmt (IfThenElse _ _ (StmtBlock _) stmt) path = return (diagnostic stmt path)
checkStmt (IfThenElse _ _ stmt1 stmt2) path = do
  stmt <- [stmt1, stmt2]
  return (diagnostic stmt path)
checkStmt (While _ _ (StmtBlock _)) _ = mzero
checkStmt (While _ _ stmt) path = return (diagnostic stmt path)
checkStmt (BasicFor _ _ _ _ (StmtBlock _)) _ = mzero
checkStmt (BasicFor _ _ _ _ stmt) path = return (diagnostic stmt path)
checkStmt (EnhancedFor _ _ _ _ _ (StmtBlock _)) _ = mzero
checkStmt (EnhancedFor _ _ _ _ _ stmt) path = return (diagnostic stmt path)
checkStmt (Do _ (StmtBlock _) _) _ = mzero
checkStmt (Do _ stmt _) path = return (diagnostic stmt path)
checkStmt _ _ = mzero

checkStatements :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
checkStatements (_, methodBody) path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (Do (StmtBlock _) _) = mzero
    checkStatement (Do _ _) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A Do-Part contains no braces." dummySourceSpan path)
    checkStatement (While _ (StmtBlock _)) = mzero
    checkStatement (While _ _) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A While-Part contains no braces." dummySourceSpan path)
    checkStatement (BasicFor _ _ _ (StmtBlock _)) = mzero
    checkStatement (BasicFor {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A For-Part contains no braces." dummySourceSpan path)
    checkStatement (EnhancedFor _ _ _ _ (StmtBlock _)) = mzero
    checkStatement (EnhancedFor {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A ForEach-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThen _ _ (StmtBlock _)) = mzero
    checkStatement (IfThen {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThen-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThenElse _ _ (StmtBlock _) (StmtBlock _)) = mzero
    checkStatement (IfThenElse _ _ _ (StmtBlock _)) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThenElse-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThenElse _ _ (StmtBlock _) _) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThenElse-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThenElse {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThenElse-Part contains no braces." dummySourceSpan path)
    checkStatement _ = mzero
