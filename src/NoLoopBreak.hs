module NoLoopBreak where

import AST (extractMethods)
import Control.Monad.Reader
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
  runReader (checkStatement stmt) (methodName, path)

checkStatement :: Stmt -> Reader (String, FilePath) [Diagnostic]
checkStatement (While _ stmt) = checkStmt stmt
checkStatement (Do stmt _) = checkStmt stmt
checkStatement (BasicFor _ _ _ stmt) = checkStmt stmt
checkStatement (EnhancedFor _ _ _ _ stmt) = checkStmt stmt
checkStatement _ = return []

checkNestedStatement :: Stmt -> Reader (String, FilePath) [Diagnostic]
checkNestedStatement (StmtBlock (Block block)) = extractLoopBody block
checkNestedStatement (IfThen _ stmt) = checkStmt stmt
checkNestedStatement (IfThenElse _ stmt1 stmt2) = do
  stm1 <- checkStmt stmt1
  stm2 <- checkStmt stmt2
  return (stm1 ++ stm2)
checkNestedStatement stmt = checkStatement stmt

extractLoopBody :: [BlockStmt] -> Reader (String, FilePath) [Diagnostic]
extractLoopBody ((BlockStmt block) : xs) = do
  stmtblock <- checkStmt block
  elb <- extractLoopBody xs
  return (stmtblock ++ elb)
extractLoopBody ((LocalClass _) : xs) = extractLoopBody xs
extractLoopBody ((LocalVars {}) : xs) = extractLoopBody xs
extractLoopBody [] = return []

checkStmt :: Stmt -> Reader (String, FilePath) [Diagnostic]
checkStmt (Return _) = do
  (methodName, path) <- ask
  return [methodDiagnostic methodName "Exit Loop with return." path]
checkStmt (Break _) = do
  (methodName, path) <- ask
  return [methodDiagnostic methodName "Exit Loop with break." path]
checkStmt stmt = checkNestedStatement stmt
