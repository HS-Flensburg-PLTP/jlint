module Language.Java.Rules.UseElse (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( Block (Block),
    BlockStmt (BlockStmt),
    CompilationUnit,
    Stmt (..),
  )
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  checkIfWithoutElse cUnit path ++ checkCodeAfterIfThenElse cUnit path

checkIfWithoutElse :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkIfWithoutElse cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (IfThen range _ stmt) = do
      if doesAlwaysExit stmt
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.UseElse"
                "Hier bitte ein `else` verwenden, damit sofort klar ist, dass der restliche Code nur ausgeführt wird, wenn die Bedingung nicht erfüllt ist."
                range
                path
            )
        else mzero
    checkStmt _ = mzero

checkCodeAfterIfThenElse :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkCodeAfterIfThenElse cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks (BlockStmt (IfThenElse range _ thenStmt _) : _ : _) =
      if doesAlwaysExit thenStmt
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.UseElse"
                "Der `then`-Zweig der `if`- verlässt immer die Methode. Daher sollte nach der `if`-Anweisung keine weitere Anweisung folgen."
                range
                path
            )
        else mzero
    checkBlocks _ = mzero

doesAlwaysExit :: Stmt -> Bool
doesAlwaysExit (Return _) = True
doesAlwaysExit (Throw _) = True
doesAlwaysExit (StmtBlock (Block [blockStmt])) = doesBlockAlwaysExit blockStmt
-- Currently not supported
doesAlwaysExit (StmtBlock _) = False
doesAlwaysExit (IfThen _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (IfThenElse _ _ stmt1 stmt2) = doesAlwaysExit stmt1 && doesAlwaysExit stmt2
doesAlwaysExit (While _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (BasicFor _ _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (EnhancedFor _ _ _ _ stmt) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Switch {}) = False
doesAlwaysExit (Do stmt _) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Synchronized _ _) = False
-- Currently not supported
doesAlwaysExit (Try {}) = False
doesAlwaysExit (Labeled _ stmt) = doesAlwaysExit stmt
doesAlwaysExit _ = False

doesBlockAlwaysExit :: BlockStmt -> Bool
doesBlockAlwaysExit (BlockStmt stmt) = doesAlwaysExit stmt
doesBlockAlwaysExit _ = False
