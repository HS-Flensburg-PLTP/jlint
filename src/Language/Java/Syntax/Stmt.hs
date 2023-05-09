module Language.Java.Syntax.Stmt (hasNoSideEffect, sourceSpan) where

import Language.Java.SourceSpan (SourceSpan, dummySourceSpan)
import Language.Java.Syntax (Stmt (..))

-- Identifies statements that do not cause side effects concerning class attributes
hasNoSideEffect :: Stmt -> Bool
hasNoSideEffect (StmtBlock {}) = False
hasNoSideEffect (IfThen {}) = False
hasNoSideEffect (IfThenElse {}) = False
hasNoSideEffect (While {}) = False
hasNoSideEffect (BasicFor {}) = False
hasNoSideEffect (EnhancedFor {}) = False
hasNoSideEffect Empty = False
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
hasNoSideEffect (Labeled _ stmt) = hasNoSideEffect stmt

sourceSpan :: Stmt -> SourceSpan
sourceSpan (IfThen span _ _) = span
sourceSpan (IfThenElse span _ _ _) = span
sourceSpan (While span _ _) = span
sourceSpan (BasicFor span _ _ _ _) = span
sourceSpan (EnhancedFor span _ _ _ _ _) = span
sourceSpan (ExpStmt span _) = span
sourceSpan (Do span _ _) = span
sourceSpan (Break span _) = span
sourceSpan (Return span _) = span
sourceSpan (Try span _ _ _ _) = span
sourceSpan _ = dummySourceSpan
