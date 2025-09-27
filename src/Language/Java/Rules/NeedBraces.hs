module Language.Java.Rules.NeedBraces (check) where

import Control.Monad (mplus, mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan, sourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = universeBi cUnit >>= checkStmt
  where
    checkStmt :: Stmt Parsed -> [RDF.Diagnostic]
    checkStmt (IfThen _ _ thenStmt) = checkConditionalThen thenStmt path
    checkStmt (IfThenElse _ _ thenStmt elseStmt) =
      checkConditionalThen thenStmt path `mplus` checkConditionalElse elseStmt path
    checkStmt (While span _ bodyStmt) = checkLoopBody bodyStmt span path
    checkStmt (BasicFor span _ _ _ bodyStmt) = checkLoopBody bodyStmt span path
    checkStmt (EnhancedFor span _ _ _ _ bodyStmt) = checkLoopBody bodyStmt span path
    checkStmt (Do span bodyStmt _) = checkLoopBody bodyStmt span path
    checkStmt _ = mzero

ruleName :: String
ruleName = "Language.Java.Rules.NeedBraces"

checkConditionalThen :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkConditionalThen = checkConditionalBody

checkConditionalElse :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkConditionalElse (IfThen {}) _ = mzero
checkConditionalElse (IfThenElse {}) _ = mzero
checkConditionalElse stmt path = checkConditionalBody stmt path

checkConditionalBody :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkConditionalBody (StmtBlock _) _ = mzero
checkConditionalBody (Empty _) _ = mzero
checkConditionalBody stmt path =
  return (RDF.rangeDiagnostic ruleName bracesMessage (sourceSpan stmt) path)

checkLoopBody :: Stmt Parsed -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkLoopBody (StmtBlock _) _ _ = mzero
checkLoopBody (Empty _) loopSpan path =
  return
    ( RDF.rangeDiagnostic
        ruleName
        ["Schleifen sollten Anweisungen enthalten."]
        loopSpan
        path
    )
checkLoopBody stmt _ path =
  return (RDF.rangeDiagnostic ruleName bracesMessage (sourceSpan stmt) path)

bracesMessage :: [String]
bracesMessage =
  ["Blöcke sollten auch dann geklammert werden, wenn sie weniger als zwei Anweisungen enthalten."]
