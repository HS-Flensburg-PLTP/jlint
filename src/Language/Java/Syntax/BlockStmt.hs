module Language.Java.Syntax.BlockStmt (name) where

import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax (BlockStmt (..), Stmt (..))

name :: BlockStmt -> String
name (BlockStmt stmt) = stmtName stmt
name (LocalClass _) = "lokale Klasse"
name (LocalVars {}) = "Variablendeklaration"

stmtName :: Stmt -> String
stmtName (Return _) = "return"
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
