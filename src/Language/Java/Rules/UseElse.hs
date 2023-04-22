module Language.Java.Rules.UseElse (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (intercalate)
import Language.Java.Syntax
  ( Block (Block),
    BlockStmt (BlockStmt),
    CompilationUnit,
    Stmt (..),
  )
import Language.Java.Syntax.BlockStmt as BlockStmt (name)
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  checkIfWithoutElse cUnit path ++ checkCodeAfterIfThenElse cUnit path

checkIfWithoutElse :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkIfWithoutElse cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks [BlockStmt _ (IfThen {})] = mzero
    checkBlocks (BlockStmt _ (IfThen range _ stmt) : _) = do
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
    checkBlocks _ = mzero

checkCodeAfterIfThenElse :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkCodeAfterIfThenElse cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks [BlockStmt _ (IfThenElse {})] = mzero
    checkBlocks (BlockStmt _ (IfThenElse range _ thenStmt elseStmt) : stmts) =
      if doesAlwaysExit thenStmt || doesAlwaysExit elseStmt
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.UseElse"
                (message stmts)
                range
                path
            )
        else mzero
    checkBlocks _ = mzero

message :: [BlockStmt] -> String
message blockStmts =
  "Der `then`- oder der `else`-Zweig der `if`-Anweisung verlässt immer die Methode. Daher sollte nach der gesamten `if`-Anweisung keine weitere Anweisung folgen. Auf die `if`-Anweisung folgen: " ++ intercalate ", " (map BlockStmt.name blockStmts)

doesAlwaysExit :: Stmt -> Bool
doesAlwaysExit (Return _ _) = True
doesAlwaysExit (Throw _) = True
doesAlwaysExit (StmtBlock (Block [])) = False
doesAlwaysExit (StmtBlock (Block blocks)) = doesBlockAlwaysExit (last blocks)
doesAlwaysExit (IfThen _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (IfThenElse _ _ stmt1 stmt2) = doesAlwaysExit stmt1 && doesAlwaysExit stmt2
doesAlwaysExit (While _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (BasicFor _ _ _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (EnhancedFor _ _ _ _ _ stmt) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Switch {}) = False
doesAlwaysExit (Do _ stmt _) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Synchronized _ _) = False
-- Currently not supported
doesAlwaysExit (Try {}) = False
doesAlwaysExit (Labeled _ stmt) = doesAlwaysExit stmt
doesAlwaysExit _ = False

doesBlockAlwaysExit :: BlockStmt -> Bool
doesBlockAlwaysExit (BlockStmt _ stmt) = doesAlwaysExit stmt
doesBlockAlwaysExit _ = False
