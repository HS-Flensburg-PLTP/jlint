module Language.Java.Syntax.BlockStmt
  ( name,
    hasNoSideEffect,
  )
where

import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax (BlockStmt (..), Stmt (..))
import qualified Language.Java.Syntax.Stmt as Stmt
import qualified Language.Java.Syntax.VarDecl as VarDecl

name :: BlockStmt -> String
name (BlockStmt _ stmt) = stmtName stmt
name (LocalClass _) = "lokale Klasse"
name (LocalVars {}) = "Variablendeklaration"

stmtName :: Stmt -> String
stmtName (Return _ _) = "return"
stmtName (Throw _) = "throw"
stmtName (StmtBlock _) = "block"
stmtName (IfThen {}) = "if-then"
stmtName (IfThenElse {}) = "if-then-else"
stmtName (While {}) = "while"
stmtName (BasicFor {}) = "for"
stmtName (EnhancedFor {}) = "foreach"
stmtName (Switch {}) = "switch"
stmtName (Do {}) = "do-while"
stmtName (Synchronized {}) = "sychronized"
stmtName (Try {}) = "try"
stmtName (Labeled {}) = "label"
stmtName Empty = "leere Anweisung"
stmtName (ExpStmt _ exp) = prettyPrint exp
stmtName (Assert {}) = "assert"
stmtName (Break {}) = "break"
stmtName (Continue {}) = "continue"

hasNoSideEffect :: BlockStmt -> Bool
hasNoSideEffect (BlockStmt _ stmt) = Stmt.hasNoSideEffect stmt
hasNoSideEffect (LocalClass _) = False
hasNoSideEffect (LocalVars _ _ _ varDecls) = all VarDecl.hasNoSideEffect varDecls
