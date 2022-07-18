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
checkStatement (While _ (StmtBlock (Block block))) = extractLoopBody block
checkStatement (Do (StmtBlock (Block block)) _) = extractLoopBody block
checkStatement (BasicFor _ _ _ (StmtBlock (Block block))) = extractLoopBody block
checkStatement (EnhancedFor _ _ _ _ (StmtBlock (Block block))) = extractLoopBody block
checkStatement _ = return []

extractLoopBody :: [BlockStmt] -> Reader (String, FilePath) [Diagnostic]
extractLoopBody ((BlockStmt block) : xs) = do
  (methodName, path) <- ask
  elb <- extractLoopBody xs
  return (checkStmt block methodName path ++ elb)
extractLoopBody ((LocalClass _) : xs) = extractLoopBody xs
extractLoopBody ((LocalVars {}) : xs) = extractLoopBody xs
extractLoopBody [] = return []

checkStmt :: Stmt -> String -> FilePath -> [Diagnostic]
checkStmt (Return _) methodName path = [methodDiagnostic methodName "Exit Loop with return." path]
checkStmt (Break _) methodName path = [methodDiagnostic methodName "Exit Loop with break." path]
checkStmt _ _ _ = []
