module NoLoopBreak where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods path

checkStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (While _ (StmtBlock (Block block))) = extractLoopBody block
    checkStatement (Do (StmtBlock (Block block)) _) = extractLoopBody block
    checkStatement (BasicFor _ _ _ (StmtBlock (Block block))) = extractLoopBody block
    checkStatement (EnhancedFor _ _ _ _ (StmtBlock (Block block))) = extractLoopBody block
    checkStatement _ = mzero
    extractLoopBody ((BlockStmt block) : xs) = checkStmt block ++ extractLoopBody xs
    extractLoopBody ((LocalClass _) : xs) = extractLoopBody xs
    extractLoopBody ((LocalVars {}) : xs) = extractLoopBody xs
    extractLoopBody [] = mzero
    checkStmt (Return _) = return (methodDiagnostic methodName "Exit Loop with return." path)
    checkStmt (Break _) = return (methodDiagnostic methodName "Exit Loop with break." path)
    checkStmt _ = []
