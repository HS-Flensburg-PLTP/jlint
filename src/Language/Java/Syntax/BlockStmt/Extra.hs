module Language.Java.Syntax.BlockStmt.Extra
  ( name,
    hasNoSideEffect,
  )
where

import Language.Java.Pretty (PrettyExtension)
import Language.Java.Syntax (BlockStmt (..))
import qualified Language.Java.Syntax.Stmt.Extra as Stmt.Extra
import qualified Language.Java.Syntax.VarDecl as VarDecl.Extra

name :: PrettyExtension p => BlockStmt p -> String
name (BlockStmt stmt) = Stmt.Extra.name stmt
name (LocalClass _) = "lokale Klasse"
name (LocalVars {}) = "Variablendeklaration"

hasNoSideEffect :: BlockStmt p -> Bool
hasNoSideEffect (BlockStmt stmt) = Stmt.Extra.hasNoSideEffect stmt
hasNoSideEffect (LocalClass _) = False
hasNoSideEffect (LocalVars _ _ _ varDecls) = all VarDecl.Extra.hasNoSideEffect varDecls
