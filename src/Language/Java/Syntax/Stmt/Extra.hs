module Language.Java.Syntax.Stmt.Extra
  ( name,
    hasNoSideEffect,
  )
where

import Language.Java.Pretty (PrettyExtension, prettyPrint)
import Language.Java.Syntax (Stmt (..))
import qualified Language.Java.Syntax.Exp.Extra as Exp.Extra

name :: PrettyExtension p => Stmt p -> String
name (Return _ _) = "return"
name (Throw _ _) = "throw"
name (StmtBlock _) = "block"
name (IfThen {}) = "if-then"
name (IfThenElse {}) = "if-then-else"
name (While {}) = "while"
name (BasicFor {}) = "for"
name (EnhancedFor {}) = "foreach"
name (Switch {}) = "switch"
name (Do {}) = "do-while"
name (Synchronized {}) = "sychronized"
name (Try {}) = "try"
name (Labeled {}) = "label"
name (Empty _) = "leere Anweisung"
name (ExpStmt _ exp) = prettyPrint exp
name (Assert {}) = "assert"
name (Break {}) = "break"
name (Continue {}) = "continue"

hasNoSideEffect :: Stmt p -> Bool
hasNoSideEffect (StmtBlock {}) = False
hasNoSideEffect (IfThen {}) = False
hasNoSideEffect (IfThenElse {}) = False
hasNoSideEffect (While {}) = False
hasNoSideEffect (BasicFor {}) = False
hasNoSideEffect (EnhancedFor {}) = False
hasNoSideEffect (Empty {}) = False
hasNoSideEffect (ExpStmt _ exp) = Exp.Extra.hasNoSideEffect exp
hasNoSideEffect (Assert {}) = False
hasNoSideEffect (Switch {}) = False
hasNoSideEffect (Do {}) = False
hasNoSideEffect (Break {}) = True
hasNoSideEffect (Continue {}) = True
hasNoSideEffect (Return {}) = True
hasNoSideEffect (Synchronized {}) = False
hasNoSideEffect (Throw {}) = False
hasNoSideEffect (Try {}) = False
hasNoSideEffect (Labeled _ _ stmt) = hasNoSideEffect stmt
