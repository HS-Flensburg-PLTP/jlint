module Language.Java.Syntax.Stmt (hasNoSideEffect, isBreak, isReturn) where

import Language.Java.Syntax (Stmt (..))

-- Identifies statements that do not cause side effects concerning class attributes
hasNoSideEffect :: Stmt p -> Bool
hasNoSideEffect (StmtBlock {}) = False
hasNoSideEffect (IfThen {}) = False
hasNoSideEffect (IfThenElse {}) = False
hasNoSideEffect (While {}) = False
hasNoSideEffect (BasicFor {}) = False
hasNoSideEffect (EnhancedFor {}) = False
hasNoSideEffect (Empty _) = False
hasNoSideEffect (ExpStmt {}) = False
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

isReturn :: Stmt p -> Bool
isReturn (Return _ _) = True
isReturn _ = False

isBreak :: Stmt p -> Bool
isBreak (Break _ _) = True
isBreak _ = False
