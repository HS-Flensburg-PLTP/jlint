module Language.Java.Rules.NoLoopBreak where

import Control.Monad.Reader
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
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
checkStatement (While _ stmt) = checkLoop stmt
checkStatement (Do stmt _) = checkLoop stmt
checkStatement (BasicFor _ _ _ stmt) = checkLoop stmt
checkStatement (EnhancedFor _ _ _ _ stmt) = checkLoop stmt
checkStatement _ = return []

checkLoop :: Stmt -> Reader (String, FilePath) [Diagnostic]
checkLoop (StmtBlock (Block block)) = extractLoopBody block
checkLoop (IfThen _ _ stmt) = checkLoop stmt
checkLoop (IfThenElse _ stmt1 stmt2) = do
  stm1 <- checkLoop stmt1
  stm2 <- checkLoop stmt2
  return (stm1 ++ stm2)
checkLoop (Return _) = do
  (methodName, path) <- ask
  return [methodDiagnostic methodName "Exit Loop with return" path]
checkLoop (Break _) = do
  (methodName, path) <- ask
  return [methodDiagnostic methodName "Exit Loop with break" path]
checkLoop stmt = checkStatement stmt

extractLoopBody :: [BlockStmt] -> Reader (String, FilePath) [Diagnostic]
extractLoopBody ((BlockStmt block) : xs) = do
  stmtblock <- checkLoop block
  elb <- extractLoopBody xs
  return (stmtblock ++ elb)
extractLoopBody ((LocalClass _) : xs) = extractLoopBody xs
extractLoopBody ((LocalVars {}) : xs) = extractLoopBody xs
extractLoopBody [] = return []
