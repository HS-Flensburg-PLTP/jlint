module Language.Java.Rules.UseElse (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (intercalate)
import Language.Java.Syntax
import qualified Language.Java.Syntax.BlockStmt.Extra as BlockStmt.Extra
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  checkIfWithoutElse cUnit path ++ checkCodeAfterIfThenElse cUnit path

checkIfWithoutElse :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkIfWithoutElse cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks [BlockStmt (IfThen {})] = mzero
    checkBlocks (BlockStmt (IfThen range _ stmt) : _) = do
      if doesAlwaysExit stmt
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.UseElse"
                [ "Hier bitte ein",
                  Markdown.code "else",
                  "verwenden, damit sofort klar ist, dass der restliche Code nur ausgeführt wird, wenn die Bedingung nicht erfüllt ist."
                ]
                range
                path
            )
        else mzero
    checkBlocks _ = mzero

checkCodeAfterIfThenElse :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkCodeAfterIfThenElse cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks [BlockStmt (IfThenElse {})] = mzero
    checkBlocks (BlockStmt (IfThenElse range _ thenStmt elseStmt) : stmts) =
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

message :: [BlockStmt Parsed] -> [String]
message blockStmts =
  [ "Der",
    Markdown.code "then" ++ "-",
    "oder der",
    Markdown.code "else" ++ "-Zweig der",
    Markdown.code "if" ++ "-Anweisung",
    "verlässt immer die Methode. Daher sollte nach der gesamten",
    Markdown.code "if" ++ "-Anweisung",
    "keine weitere Anweisung folgen. Auf die",
    Markdown.code "if" ++ "-Anweisung",
    "folgen: ",
    intercalate ", " (map BlockStmt.Extra.name blockStmts)
  ]

doesAlwaysExit :: Stmt Parsed -> Bool
doesAlwaysExit (Return _ _) = True
doesAlwaysExit (Throw _ _) = True
doesAlwaysExit (StmtBlock (Block _ [])) = False
doesAlwaysExit (StmtBlock (Block _ blocks)) = doesBlockAlwaysExit (last blocks)
doesAlwaysExit (IfThen _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (IfThenElse _ _ stmt1 stmt2) = doesAlwaysExit stmt1 && doesAlwaysExit stmt2
doesAlwaysExit (While _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (BasicFor _ _ _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit (EnhancedFor _ _ _ _ _ stmt) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Switch {}) = False
doesAlwaysExit (Do _ stmt _) = doesAlwaysExit stmt
-- Currently not supported
doesAlwaysExit (Synchronized {}) = False
-- Currently not supported
doesAlwaysExit (Try {}) = False
doesAlwaysExit (Labeled _ _ stmt) = doesAlwaysExit stmt
doesAlwaysExit _ = False

doesBlockAlwaysExit :: BlockStmt Parsed -> Bool
doesBlockAlwaysExit (BlockStmt stmt) = doesAlwaysExit stmt
doesBlockAlwaysExit _ = False
