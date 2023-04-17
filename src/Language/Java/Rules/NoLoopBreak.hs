module Language.Java.Rules.NoLoopBreak where

import Control.Monad.Reader
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods path

checkStatements :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  runReader (checkStatement stmt) (methodName, path)

checkStatement :: Stmt -> Reader (String, FilePath) [RDF.Diagnostic]
checkStatement (While _ stmt) = checkLoop stmt
checkStatement (Do stmt _) = checkLoop stmt
checkStatement (BasicFor _ _ _ stmt) = checkLoop stmt
checkStatement (EnhancedFor _ _ _ _ stmt) = checkLoop stmt
checkStatement _ = return []

checkLoop :: Stmt -> Reader (String, FilePath) [RDF.Diagnostic]
checkLoop (StmtBlock (Block block)) = extractLoopBody block
checkLoop (IfThen _ _ stmt) = checkLoop stmt
checkLoop (IfThenElse _ _ stmt1 stmt2) = do
  stm1 <- checkLoop stmt1
  stm2 <- checkLoop stmt2
  return (stm1 ++ stm2)
checkLoop (Return _) = do
  (_, path) <- ask
  return [RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Exit Loop with return" dummySourceSpan path]
checkLoop (Break _) = do
  (_, path) <- ask
  return [RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Exit Loop with break" dummySourceSpan path]
checkLoop stmt = checkStatement stmt

extractLoopBody :: [BlockStmt] -> Reader (String, FilePath) [RDF.Diagnostic]
extractLoopBody ((BlockStmt _ block) : xs) = do
  stmtblock <- checkLoop block
  elb <- extractLoopBody xs
  return (stmtblock ++ elb)
extractLoopBody ((LocalClass _) : xs) = extractLoopBody xs
extractLoopBody ((LocalVars {}) : xs) = extractLoopBody xs
extractLoopBody [] = return []
