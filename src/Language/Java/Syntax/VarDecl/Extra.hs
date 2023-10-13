module Language.Java.Syntax.VarDecl.Extra (hasNoSideEffect, isCheap) where

import Language.Java.Syntax (VarDecl (..))
import qualified Language.Java.Syntax.VarInit.Extra as VarInit.Extra

hasNoSideEffect :: VarDecl p -> Bool
hasNoSideEffect (VarDecl _ _ Nothing) = True
hasNoSideEffect (VarDecl _ _ (Just varInit)) = VarInit.Extra.hasNoSideEffect varInit

isCheap :: VarDecl p -> Bool
isCheap (VarDecl _ _ Nothing) = True
isCheap (VarDecl _ _ (Just varInit)) = VarInit.Extra.isCheap varInit
