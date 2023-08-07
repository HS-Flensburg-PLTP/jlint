module Language.Java.Rules.NoLoopBreak (check) where

import Control.Monad.Reader
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods path

checkStatements :: (String, MethodBody Parsed) -> FilePath -> [RDF.Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  runReader (checkStatement stmt) (methodName, path)

checkStatement :: Stmt Parsed -> Reader (String, FilePath) [RDF.Diagnostic]
checkStatement (While _ _ stmt) = checkLoop stmt
checkStatement (Do _ stmt _) = checkLoop stmt
checkStatement (BasicFor _ _ _ _ stmt) = checkLoop stmt
checkStatement (EnhancedFor _ _ _ _ _ stmt) = checkLoop stmt
checkStatement _ = return []

checkLoop :: Stmt Parsed -> Reader (String, FilePath) [RDF.Diagnostic]
checkLoop (StmtBlock (Block block)) = extractLoopBody block
checkLoop (IfThen _ _ stmt) = checkLoop stmt
checkLoop (IfThenElse _ _ stmt1 stmt2) = do
  stm1 <- checkLoop stmt1
  stm2 <- checkLoop stmt2
  return (stm1 ++ stm2)
checkLoop (Return _ _) = do
  (_, path) <- ask
  return [RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Exit Loop with return" dummySourceSpan path]
checkLoop (Break _ _) = do
  (_, path) <- ask
  return [RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Exit Loop with break" dummySourceSpan path]
checkLoop stmt = checkStatement stmt

extractLoopBody :: [BlockStmt Parsed] -> Reader (String, FilePath) [RDF.Diagnostic]
extractLoopBody ((BlockStmt block) : xs) = do
  stmtblock <- checkLoop block
  elb <- extractLoopBody xs
  return (stmtblock ++ elb)
extractLoopBody ((LocalClass _) : xs) = extractLoopBody xs
extractLoopBody ((LocalVars {}) : xs) = extractLoopBody xs
extractLoopBody [] = return []
