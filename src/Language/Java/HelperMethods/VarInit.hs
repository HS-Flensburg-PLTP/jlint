module Language.Java.HelperMethods.VarInit (hasNoSideEffect) where

import qualified Language.Java.HelperMethods.Exp as Exp
import Language.Java.Syntax (ArrayInit (ArrayInit), VarInit (InitArray, InitExp))

hasNoSideEffect :: VarInit p -> Bool
hasNoSideEffect (InitExp exp) = Exp.hasNoSideEffect exp
hasNoSideEffect (InitArray (ArrayInit _ varInits)) = all hasNoSideEffect varInits
