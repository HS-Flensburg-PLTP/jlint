module Language.Java.Rules.SameExecutionsInIf where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkMethodBlocks methods path

checkMethodBlocks :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkMethodBlocks (methodName, methodBody) path = do
  blockStmts <- universeBi methodBody
  checkStmts (extractIfThenElseBlocks blockStmts)
  where
    extractIfThenElseBlocks (BlockStmt stmt) = extractStmt stmt
    extractIfThenElseBlocks _ = mzero
    checkStmts [] = []
    checkStmts list = createDiagnostic (checkAllStatements list 0 + checkAllStatements (reverseList list) 0) (length (head list))
    createDiagnostic counter listLength
      | counter > 0 && counter < listLength = [methodDiagnostic methodName ("In an if-then-else statement, " ++ show counter ++ " line(s) of code is/are the same and can be swapped out.") path]
      | counter >= listLength = [methodDiagnostic methodName "In an if-then-else statement, all lines of code are the same and can be swapped out." path]
      | otherwise = []

checkAllStatements :: [[BlockStmt]] -> Int -> Int
checkAllStatements list counter
  | all (== head (map (\(firstStmtList : _) -> firstStmtList) list)) (concatMap (take 1) list) = if all (\statements -> length statements >= 2) list then checkAllStatements (map tail list) (counter + 1) else counter + 1
  | otherwise = counter

extractStmt :: Stmt -> [[BlockStmt]]
extractStmt (IfThenElse _ _ (StmtBlock (Block blockA)) (StmtBlock (Block blockB))) = [blockA, blockB]
extractStmt (IfThenElse _ _ (StmtBlock (Block blockA)) ifThenElse) = blockA : extractStmt ifThenElse
extractStmt _ = []

reverseList :: [[BlockStmt]] -> [[BlockStmt]]
reverseList = map reverse
