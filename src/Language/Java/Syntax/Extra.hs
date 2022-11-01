module Language.Java.Syntax.Extra (blockStmtName, stmtName) where

import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax (BlockStmt (..), Stmt (..))

blockStmtName :: BlockStmt -> String
blockStmtName (BlockStmt stmt) = stmtName stmt
blockStmtName (LocalClass _) = "lokale Klasse"
blockStmtName (LocalVars {}) = "Variablendeklaration"

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
stmtName (ExpStmt exp) = prettyPrint exp
stmtName (Assert {}) = "assert"
stmtName (Break {}) = "break"
stmtName (Continue {}) = "continue"
