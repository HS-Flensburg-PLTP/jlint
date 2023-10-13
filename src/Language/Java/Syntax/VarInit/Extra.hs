module Language.Java.Syntax.VarInit.Extra (hasNoSideEffect, isCheap) where

import Language.Java.Syntax (ArrayInit (ArrayInit), VarInit (..))
import qualified Language.Java.Syntax.Exp.Extra as Exp.Extra

hasNoSideEffect :: VarInit p -> Bool
hasNoSideEffect (InitExp expr) = Exp.Extra.hasNoSideEffect expr
hasNoSideEffect (InitArray (ArrayInit _ varInits)) = all hasNoSideEffect varInits

isCheap :: VarInit p -> Bool
isCheap (InitExp expr) = Exp.Extra.isCheap expr
isCheap (InitArray (ArrayInit _ varInits)) = all isCheap varInits
