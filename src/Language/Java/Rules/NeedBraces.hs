module Language.Java.Rules.NeedBraces (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
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

diagnostic :: Stmt -> FilePath -> RDF.Diagnostic
diagnostic stmt =
  RDF.rangeDiagnostic
    "Language.Java.Rules.NeedBraces"
    "Code-Blöcke sollten auch dann geklammert werden, wenn sie weniger als zwei Anweisungen enthalten."
    (Stmt.sourceSpan stmt)
