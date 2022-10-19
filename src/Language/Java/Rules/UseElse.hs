module Language.Java.Rules.UseElse (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax (Block (..), BlockStmt (..), CompilationUnit (..), Stmt (..))
import qualified RDF as RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (IfThen range _ stmt) = do
      if doesAlwaysExit stmt
        then return (RDF.rangeDiagnostic "Language.Java.Rules.UseElse" "Always use an `else` if the code in the then branch always exits." range path)
        else mzero
    checkStmt _ = mzero

doesAlwaysExit :: Stmt -> Bool
doesAlwaysExit (Return _) = True
doesAlwaysExit (Throw _) = True
doesAlwaysExit (StmtBlock (Block [blockStmt])) = doesBlockAlwaysExit blockStmt
-- Currently not supported
doesAlwaysExit (StmtBlock _) = False
doesAlwaysExit (IfThen _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (IfThenElse _ stmt1 stmt2) = doesAlwaysExit stmt1 && doesAlwaysExit stmt2
doesAlwaysExit (While _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (BasicFor _ _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (EnhancedFor _ _ _ _ stmt) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Switch _ _ _) = False
doesAlwaysExit (Do stmt _) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Synchronized _ _) = False
-- Currently not supported
doesAlwaysExit (Try _ _ _ _) = False
doesAlwaysExit (Labeled _ stmt) = doesAlwaysExit stmt
doesAlwaysExit _ = False

doesBlockAlwaysExit :: BlockStmt -> Bool
doesBlockAlwaysExit (BlockStmt stmt) = doesAlwaysExit stmt
doesBlockAlwaysExit _ = False
