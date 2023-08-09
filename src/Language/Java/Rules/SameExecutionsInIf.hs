module Language.Java.Rules.SameExecutionsInIf (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  IfThenElse span _ stmt1 stmt2 <- universeBi cUnit
  compareStmts stmt1 stmt2 span path

compareStmts :: Stmt Parsed -> Stmt Parsed -> SourceSpan -> FilePath -> [RDF.Diagnostic]
compareStmts stmt1 stmt2 span path
  | eq IgnoreSourceSpan stmt1 stmt2 =
      return (duplicateCodeMessage span path)
compareStmts (StmtBlock (Block blockstmts@(blockstmt : _))) (StmtBlock (Block blockstmts2@(blockstmt2 : _))) span path
  | eq IgnoreSourceSpan blockstmt blockstmt2 || eq IgnoreSourceSpan (last blockstmts) (last blockstmts2) =
      return (partDuplicatedCodeMessage span path)
compareStmts stmt (StmtBlock (Block blockstmts)) span path =
  case blockstmts of
    [] -> mzero
    [blockstmt] ->
      if checkStmt blockstmt stmt
        then return (duplicateCodeMessage span path)
        else mzero
    _ ->
      if checkStmt (head blockstmts) stmt || checkStmt (last blockstmts) stmt
        then return (partDuplicatedCodeMessage span path)
        else mzero
compareStmts (StmtBlock (Block blockstmts)) stmt span path =
  case blockstmts of
    [] -> mzero
    [blockstmt] ->
      if checkStmt blockstmt stmt
        then return (duplicateCodeMessage span path)
        else mzero
    _ ->
      if checkStmt (head blockstmts) stmt || checkStmt (last blockstmts) stmt
        then return (partDuplicatedCodeMessage span path)
        else mzero
compareStmts _ _ _ _ = mzero

checkStmt :: BlockStmt Parsed -> Stmt Parsed -> Bool
checkStmt (BlockStmt _ stmt2) stmt =
  eq IgnoreSourceSpan stmt stmt2
checkStmt _ _ = False

duplicateCodeMessage :: SourceSpan -> FilePath -> RDF.Diagnostic
duplicateCodeMessage = RDF.rangeDiagnostic "Language.Java.Rules.SameExecutionsInIf" "In dem If Else befindet sich identischer Code, welcher ausgelagert werden kann."

partDuplicatedCodeMessage :: SourceSpan -> FilePath -> RDF.Diagnostic
partDuplicatedCodeMessage = RDF.rangeDiagnostic "Language.Java.Rules.SameExecutionsInIf" "In dem If Else befindet sich zum Teil identischer Code, welcher ausgelagert werden kann."
