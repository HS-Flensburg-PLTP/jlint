module Language.Java.Syntax.VarDecl
  ( isInitialized,
    isInitializedWithoutSideEffect,
    ident,
  )
where

import Language.Java.Syntax (Ident, VarDecl (VarDecl), VarDeclId (VarDeclArray, VarId))
import qualified Language.Java.Syntax.VarInit as VarInit

isInitialized :: VarDecl -> Bool
isInitialized (VarDecl _ (Just _)) = True
isInitialized (VarDecl _ Nothing) = False

isInitializedWithoutSideEffect :: VarDecl -> Bool
isInitializedWithoutSideEffect (VarDecl _ Nothing) = False
isInitializedWithoutSideEffect (VarDecl _ (Just varInit)) = VarInit.hasNoSideEffect varInit

ident :: VarDecl -> Ident
ident (VarDecl varDeclId _) = varDeclIdIdent varDeclId

varDeclIdIdent :: VarDeclId -> Ident
varDeclIdIdent (VarDeclArray varDeclId) = varDeclIdIdent varDeclId
varDeclIdIdent (VarId ident) = ident
