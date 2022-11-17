module Language.Java.Purity (hasNoSideEffectExp, hasNoSideEffect) where

import Language.Java.Syntax (Block (..), Exp (..), Stmt (..))

-- Identifies statements that do not cause side effects concerning class attributes
hasNoSideEffect :: Stmt -> Bool
hasNoSideEffect (StmtBlock (Block _)) = False
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

hasNoSideEffectExp :: Exp -> Bool
hasNoSideEffectExp (Lit _) = True
hasNoSideEffectExp (ClassLit _) = False
hasNoSideEffectExp This = False
hasNoSideEffectExp (ThisClass {}) = False
hasNoSideEffectExp (InstanceCreation {}) = False
hasNoSideEffectExp (QualInstanceCreation {}) = False
hasNoSideEffectExp (ArrayCreate {}) = False
hasNoSideEffectExp (ArrayCreateInit {}) = False
hasNoSideEffectExp (FieldAccess {}) = True
hasNoSideEffectExp (MethodInv {}) = False
hasNoSideEffectExp (ArrayAccess {}) = False
hasNoSideEffectExp (ExpName {}) = True
hasNoSideEffectExp (PostIncrement _) = False
hasNoSideEffectExp (PostDecrement _) = False
hasNoSideEffectExp (PreIncrement _) = False
hasNoSideEffectExp (PreDecrement _) = False
hasNoSideEffectExp (PrePlus {}) = False
hasNoSideEffectExp (PreMinus {}) = False
hasNoSideEffectExp (PreBitCompl {}) = False
hasNoSideEffectExp (PreNot {}) = False
hasNoSideEffectExp (SwitchExp {}) = False
hasNoSideEffectExp (Cast {}) = False
hasNoSideEffectExp (BinOp leftExp _ rightExp) = hasNoSideEffectExp leftExp && hasNoSideEffectExp rightExp
hasNoSideEffectExp (InstanceOf {}) = False
hasNoSideEffectExp (Cond {}) = False
hasNoSideEffectExp (Assign {}) = False
hasNoSideEffectExp (Lambda {}) = False
hasNoSideEffectExp (MethodRef {}) = False