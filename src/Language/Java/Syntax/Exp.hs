module Language.Java.Syntax.Exp (hasNoSideEffect, isDataCreation, isCast) where

import Language.Java.Syntax (Exp (..))

isCast :: Exp p -> Bool
isCast (Cast {}) = True
isCast _ = False

isDataCreation :: Exp p -> Bool
isDataCreation (ArrayCreate {}) = True
isDataCreation (InstanceCreation {}) = True
isDataCreation (QualInstanceCreation {}) = True
isDataCreation _ = False

hasNoSideEffect :: Exp p -> Bool
hasNoSideEffect (Lit _) = True
hasNoSideEffect (ClassLit _ _) = False
hasNoSideEffect (This _) = False
hasNoSideEffect (ThisClass {}) = False
hasNoSideEffect (InstanceCreation {}) = False
hasNoSideEffect (QualInstanceCreation {}) = False
hasNoSideEffect (ArrayCreate {}) = False
hasNoSideEffect (ArrayCreateInit {}) = False
hasNoSideEffect (FieldAccess {}) = True
hasNoSideEffect (MethodInv {}) = False
hasNoSideEffect (ArrayAccess {}) = False
hasNoSideEffect (ExpName {}) = True
hasNoSideEffect (PostIncrement _ _) = False
hasNoSideEffect (PostDecrement _ _) = False
hasNoSideEffect (PreIncrement _ _) = False
hasNoSideEffect (PreDecrement _ _) = False
hasNoSideEffect (PrePlus {}) = False
hasNoSideEffect (PreMinus {}) = False
hasNoSideEffect (PreBitCompl {}) = False
hasNoSideEffect (PreNot {}) = False
hasNoSideEffect (SwitchExp {}) = False
hasNoSideEffect (Cast {}) = False
hasNoSideEffect (BinOp _ leftExp _ rightExp) = hasNoSideEffect leftExp && hasNoSideEffect rightExp
hasNoSideEffect (InstanceOf {}) = False
hasNoSideEffect (Cond {}) = False
hasNoSideEffect (Assign {}) = False
hasNoSideEffect (Lambda {}) = False
hasNoSideEffect (MethodRef {}) = False
