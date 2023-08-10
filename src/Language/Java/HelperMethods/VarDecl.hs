module Language.Java.HelperMethods.VarDecl
  ( hasNoSideEffect,
  )
where

import qualified Language.Java.HelperMethods.VarInit as VarInit
import Language.Java.Syntax (VarDecl (..))

hasNoSideEffect :: VarDecl p -> Bool
hasNoSideEffect (VarDecl _ _ Nothing) = True
hasNoSideEffect (VarDecl _ _ (Just varInit)) = VarInit.hasNoSideEffect varInit
