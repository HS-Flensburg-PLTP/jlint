module Language.Java.Syntax.Exp.Extra (hasNoSideEffect, isCheap) where

import Language.Java.Syntax (Exp (..))

hasNoSideEffect :: Exp p -> Bool
hasNoSideEffect (Lit _) = True
hasNoSideEffect (ClassLit _ _) = True
hasNoSideEffect (This _) = True
hasNoSideEffect (ThisClass {}) = True
hasNoSideEffect (InstanceCreation _ _ _ arguments _) = all hasNoSideEffect arguments
hasNoSideEffect (QualInstanceCreation _ _ _ _ arguments _) = all hasNoSideEffect arguments
hasNoSideEffect (ArrayCreate _ _ exps _) = all hasNoSideEffect exps
hasNoSideEffect (ArrayCreateInit {}) = True
hasNoSideEffect (FieldAccess {}) = True
hasNoSideEffect (MethodInv {}) = False
hasNoSideEffect (ArrayAccess {}) = True
hasNoSideEffect (ExpName {}) = True
hasNoSideEffect (PostIncrement _ _) = False
hasNoSideEffect (PostDecrement _ _) = False
hasNoSideEffect (PreIncrement _ _) = False
hasNoSideEffect (PreDecrement _ _) = False
hasNoSideEffect (PrePlus _ exp) = hasNoSideEffect exp
hasNoSideEffect (PreMinus _ exp) = hasNoSideEffect exp
hasNoSideEffect (PreBitCompl _ exp) = hasNoSideEffect exp
hasNoSideEffect (PreNot _ exp) = hasNoSideEffect exp
-- not implemented
hasNoSideEffect (SwitchExp {}) = False
hasNoSideEffect (Cast {}) = True
hasNoSideEffect (BinOp _ leftExp _ rightExp) = hasNoSideEffect leftExp && hasNoSideEffect rightExp
hasNoSideEffect (InstanceOf _ exp _ _) = hasNoSideEffect exp
hasNoSideEffect (Cond _ condExp thenExp elseExp) = all hasNoSideEffect [condExp, thenExp, elseExp]
hasNoSideEffect (Assign {}) = False
-- not implemented
hasNoSideEffect (Lambda {}) = True
hasNoSideEffect (MethodRef {}) = False

isCheap :: Exp p -> Bool
isCheap (Lit _) = True
isCheap (ClassLit _ _) = False
isCheap (This _) = False
isCheap (ThisClass {}) = False
isCheap (InstanceCreation {}) = False
isCheap (QualInstanceCreation {}) = False
isCheap (ArrayCreate {}) = False
isCheap (ArrayCreateInit {}) = False
isCheap (FieldAccess {}) = True
isCheap (MethodInv {}) = False
isCheap (ArrayAccess {}) = False
isCheap (ExpName {}) = True
isCheap (PostIncrement _ _) = False
isCheap (PostDecrement _ _) = False
isCheap (PreIncrement _ _) = False
isCheap (PreDecrement _ _) = False
isCheap (PrePlus {}) = False
isCheap (PreMinus {}) = False
isCheap (PreBitCompl {}) = False
isCheap (PreNot {}) = False
isCheap (SwitchExp {}) = False
isCheap (Cast {}) = False
isCheap (BinOp {}) = False
isCheap (InstanceOf {}) = False
isCheap (Cond {}) = False
isCheap (Assign {}) = False
isCheap (Lambda {}) = False
isCheap (MethodRef {}) = False
