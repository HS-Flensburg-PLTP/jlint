module SameExecutionsInIf where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkFirstStatements methods path

checkFirstStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkFirstStatements (methodName, methodBody) path = do
  blockStmtms <- universeBi methodBody
  extractFirstStmt blockStmtms
  where
    extractFirstStmt (BlockStmt (IfThenElse _ (StmtBlock (Block (x : _))) (StmtBlock (Block (y : _)))))
      | x == y = [methodDiagnostic methodName "In an if-then-else statement, the first line of code is the same and can be swapped out." path]
      | otherwise = []
    extractFirstStmt (BlockStmt (IfThenElse _ (StmtBlock (Block (x : _))) ifThenElse))
      | all (== x) (extractNestedIfThenElse ifThenElse) = [methodDiagnostic methodName "In an if-then-else statement, the first line of code is the same and can be swapped out." path]
      | otherwise = []
    extractFirstStmt _ = []
    extractNestedIfThenElse (IfThenElse _ (StmtBlock (Block (x : _))) (StmtBlock (Block (y : _)))) = [x, y]
    extractNestedIfThenElse (IfThenElse _ (StmtBlock (Block (x : _))) ifThenElse) = x : extractNestedIfThenElse ifThenElse
    extractNestedIfThenElse _ = mzero
