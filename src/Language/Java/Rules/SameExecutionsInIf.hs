module Language.Java.Rules.SameExecutionsInIf (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkMethodBlocks methods path

checkMethodBlocks :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
checkMethodBlocks (_, methodBody) path = do
  blockStmts <- universeBi methodBody
  checkStmts (extractIfThenElseBlocks blockStmts)
  where
    extractIfThenElseBlocks (BlockStmt _ stmt) = extractStmt stmt
    extractIfThenElseBlocks _ = mzero
    checkStmts [] = []
    checkStmts list = createDiagnostic (checkAllStatements list 0 + checkAllStatements (reverseList list) 0) (length (head list))
    createDiagnostic counter listLength
      | counter > 0 && counter < listLength = [RDF.rangeDiagnostic "Language.Java.Rules.SameExecutionsInIf" ("In an if-then-else statement, " ++ show counter ++ " line(s) of code is/are the same and can be swapped out.") dummySourceSpan path]
      | counter >= listLength = [RDF.rangeDiagnostic "Language.Java.Rules.SameExecutionsInIf" "In an if-then-else statement, all lines of code are the same and can be swapped out." dummySourceSpan path]
      | otherwise = []

checkAllStatements :: [[BlockStmt]] -> Int -> Int
checkAllStatements list counter
  | all (eq IgnoreSourceSpan (head (map (\(firstStmtList : _) -> firstStmtList) list))) (concatMap (take 1) list) = if all (\statements -> length statements >= 2) list then checkAllStatements (map tail list) (counter + 1) else counter + 1
  | otherwise = counter

extractStmt :: Stmt -> [[BlockStmt]]
extractStmt (IfThenElse _ _ (StmtBlock (Block blockA)) (StmtBlock (Block blockB))) = [blockA, blockB]
extractStmt (IfThenElse _ _ (StmtBlock (Block blockA)) ifThenElse) = blockA : extractStmt ifThenElse
extractStmt _ = []

reverseList :: [[BlockStmt]] -> [[BlockStmt]]
reverseList = map reverse
