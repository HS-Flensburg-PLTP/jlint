module Language.Java.Rules.NeedBraces (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Stmt as Stmt
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt path

checkStmt :: Stmt -> FilePath -> [RDF.Diagnostic]
checkStmt (IfThen _ _ stmt) path = checkConditionalBody stmt path
checkStmt (IfThenElse _ _ stmt1 stmt2) path = do
  stmt <- [stmt1, stmt2]
  checkConditionalBody stmt path
checkStmt (While span _ stmt) path = checkLoopBody stmt span path
checkStmt (BasicFor span _ _ _ stmt) path = checkLoopBody stmt span path
checkStmt (EnhancedFor span _ _ _ _ stmt) path = checkLoopBody stmt span path
checkStmt (Do span stmt _) path = checkLoopBody stmt span path
checkStmt _ _ = mzero

checkConditionalBody :: Stmt -> FilePath -> [RDF.Diagnostic]
checkConditionalBody (StmtBlock _) _ = mzero
checkConditionalBody Empty _ = mzero
checkConditionalBody stmt path = return (diagnostic bracesMessage (Stmt.sourceSpan stmt) path)

checkLoopBody :: Stmt -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkLoopBody (StmtBlock _) _ _ = mzero
checkLoopBody Empty loopSpan path = return (diagnostic emptyLoopMessage loopSpan path)
checkLoopBody stmt _ path = return (diagnostic bracesMessage (Stmt.sourceSpan stmt) path)

diagnostic :: String -> SourceSpan -> FilePath -> RDF.Diagnostic
diagnostic =
  RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces"

bracesMessage :: String
bracesMessage = "Code-Blöcke sollten auch dann geklammert werden, wenn sie weniger als zwei Anweisungen enthalten."

emptyLoopMessage :: String
emptyLoopMessage = "Schleifen sollten Anweisungen enthalten."
