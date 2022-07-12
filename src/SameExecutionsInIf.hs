module SameExecutionsInIf where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
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
    checkStmts list = createDiagnostic (checkAllStatements list 0)
    createDiagnostic counter = if counter > 0 then [methodDiagnostic methodName ("In an if-then-else statement, " ++ show counter ++ " line(s) of code is/are the same and can be swapped out.") path] else []

checkAllStatements :: [[BlockStmt]] -> Int -> Int
checkAllStatements list counter
  | all (== head (map (\(x : _) -> x) list)) (concatMap (take 1) list) =
    if all (\statements -> length statements >= 2) list then checkAllStatements (map tail list) (counter + 1) else counter + 1
  | otherwise = counter

extractStmt :: Stmt -> [[BlockStmt]]
extractStmt (IfThenElse _ (StmtBlock (Block blockA)) (StmtBlock (Block blockB))) = [blockA, blockB]
extractStmt (IfThenElse _ (StmtBlock (Block blockA)) ifThenElse) = blockA : extractStmt ifThenElse
extractStmt _ = []
