module Language.Java.Syntax.BlockStmt.Extra
  ( name,
  )
where

import Language.Java.Pretty (PrettyExtension)
import Language.Java.Syntax (BlockStmt (..))
import qualified Language.Java.Syntax.Stmt.Extra as Stmt.Extra

name :: PrettyExtension p => BlockStmt p -> String
name (BlockStmt stmt) = Stmt.Extra.name stmt
name (LocalClass _) = "lokale Klasse"
name (LocalVars {}) = "Variablendeklaration"
