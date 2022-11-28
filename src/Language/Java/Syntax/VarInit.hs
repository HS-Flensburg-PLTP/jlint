module Language.Java.Syntax.VarInit (hasNoSideEffect) where

import Language.Java.Syntax (ArrayInit (ArrayInit), VarInit (InitArray, InitExp))
import qualified Language.Java.Syntax.Exp as Exp

hasNoSideEffect :: VarInit -> Bool
hasNoSideEffect (InitExp exp) = Exp.hasNoSideEffect (exp)
hasNoSideEffect (InitArray (ArrayInit varInits)) = all hasNoSideEffect varInits
