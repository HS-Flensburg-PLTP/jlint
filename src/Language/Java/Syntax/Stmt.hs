module Language.Java.Syntax.Stmt
  ( hasNoSideEffect,
    isBreak,
    isLoop,
    isReturn,
    isSwitch,
  )
where

import Language.Java.Syntax (Stmt (..))

-- Identifies statements that do not cause side effects concerning class attributes
hasNoSideEffect :: Stmt p -> Bool
hasNoSideEffect (StmtBlock {}) = False
hasNoSideEffect (IfThen {}) = False
hasNoSideEffect (IfThenElse {}) = False
hasNoSideEffect (While {}) = False
hasNoSideEffect (BasicFor {}) = False
hasNoSideEffect (EnhancedFor {}) = False
hasNoSideEffect (Empty {}) = False
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

isBreak :: Stmt p -> Bool
isBreak (Break {}) = True
isBreak _ = False

isLoop :: Stmt p -> Bool
isLoop (While {}) = True
isLoop (Do {}) = True
isLoop (BasicFor {}) = True
isLoop (EnhancedFor {}) = True
isLoop _ = False

isReturn :: Stmt p -> Bool
isReturn (Return {}) = True
isReturn _ = False

isSwitch :: Stmt p -> Bool
isSwitch (Switch {}) = True
isSwitch _ = False
