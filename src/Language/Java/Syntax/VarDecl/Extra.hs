module Language.Java.Syntax.VarDecl.Extra (hasNoSideEffect) where

import Language.Java.Syntax (VarDecl (..))
import qualified Language.Java.Syntax.VarInit.Extra as VarInit.Extra

hasNoSideEffect :: VarDecl p -> Bool
hasNoSideEffect (VarDecl _ _ Nothing) = True
hasNoSideEffect (VarDecl _ _ (Just varInit)) = VarInit.Extra.hasNoSideEffect varInit
