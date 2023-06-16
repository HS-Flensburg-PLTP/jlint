module Language.Java.Syntax.Exp (hasNoSideEffect) where

import Language.Java.Syntax (Exp (..))

hasNoSideEffect :: Exp p -> Bool
hasNoSideEffect (Lit _ _) = True
hasNoSideEffect (ClassLit _) = False
hasNoSideEffect This = False
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
hasNoSideEffect (BinOp leftExp _ rightExp) = hasNoSideEffect leftExp && hasNoSideEffect rightExp
hasNoSideEffect (InstanceOf {}) = False
hasNoSideEffect (Cond {}) = False
hasNoSideEffect (Assign {}) = False
hasNoSideEffect (Lambda {}) = False
hasNoSideEffect (MethodRef {}) = False
