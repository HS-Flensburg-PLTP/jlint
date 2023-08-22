module Language.Java.Syntax.Stmt.Extra
  ( isLoop,
    name,
  )
where

import Language.Java.Pretty (PrettyExtension, prettyPrint)
import Language.Java.Syntax (Stmt (..))
import qualified Language.Java.Syntax.Stmt as Stmt

isLoop :: Stmt p -> Bool
isLoop stmt = Stmt.isWhile stmt || Stmt.isDo stmt || Stmt.isBasicFor stmt || Stmt.isEnhancedFor stmt

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
