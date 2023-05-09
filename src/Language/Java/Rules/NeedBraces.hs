module Language.Java.Rules.NeedBraces (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan, sourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt path

checkStmt :: Stmt -> FilePath -> [RDF.Diagnostic]
checkStmt (IfThen _ _ stmt) path = checkConditionalThen stmt path
checkStmt (IfThenElse _ _ stmt1 stmt2) path =
  checkConditionalThen stmt1 path ++ checkConditionalElse stmt2 path
checkStmt (While span _ stmt) path = checkLoopBody stmt span path
checkStmt (BasicFor span _ _ _ stmt) path = checkLoopBody stmt span path
checkStmt (EnhancedFor span _ _ _ _ stmt) path = checkLoopBody stmt span path
checkStmt (Do span stmt _) path = checkLoopBody stmt span path
checkStmt _ _ = mzero

checkConditionalThen :: Stmt -> FilePath -> [RDF.Diagnostic]
checkConditionalThen = checkConditionalBody

checkConditionalElse :: Stmt -> FilePath -> [RDF.Diagnostic]
checkConditionalElse (IfThen {}) _ = mzero
checkConditionalElse (IfThenElse {}) _ = mzero
checkConditionalElse stmt path = checkConditionalBody stmt path

checkConditionalBody :: Stmt -> FilePath -> [RDF.Diagnostic]
checkConditionalBody (StmtBlock _) _ = mzero
checkConditionalBody Empty _ = mzero
checkConditionalBody stmt path = return (diagnostic bracesMessage (sourceSpan stmt) path)

checkLoopBody :: Stmt -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkLoopBody (StmtBlock _) _ _ = mzero
checkLoopBody Empty loopSpan path = return (diagnostic emptyLoopMessage loopSpan path)
checkLoopBody stmt _ path = return (diagnostic bracesMessage (sourceSpan stmt) path)

diagnostic :: String -> SourceSpan -> FilePath -> RDF.Diagnostic
diagnostic =
  RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces"

bracesMessage :: String
bracesMessage = "Code-Blöcke sollten auch dann geklammert werden, wenn sie weniger als zwei Anweisungen enthalten."

emptyLoopMessage :: String
emptyLoopMessage = "Schleifen sollten Anweisungen enthalten."
