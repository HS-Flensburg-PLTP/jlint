module Language.Java.HelperMethods.VarDecl
  ( isInitialized,
    hasNoSideEffect,
    ident,
    varDeclIdIdent,
  )
where

import Language.Java.Syntax (Ident, VarDecl (VarDecl), VarDeclId (VarDeclArray, VarId))
import qualified Language.Java.HelperMethods.VarInit as VarInit


isInitialized :: VarDecl p -> Bool
isInitialized (VarDecl _ _ (Just _)) = True
isInitialized (VarDecl _ _ Nothing) = False

ident :: VarDecl p -> Ident
ident (VarDecl _ varDeclId _) = varDeclIdIdent varDeclId

varDeclIdIdent :: VarDeclId -> Ident
varDeclIdIdent (VarDeclArray _ varDeclId) = varDeclIdIdent varDeclId
varDeclIdIdent (VarId ident) = ident

hasNoSideEffect :: VarDecl p -> Bool
hasNoSideEffect (VarDecl _ _ Nothing) = True
hasNoSideEffect (VarDecl _ _ (Just varInit)) = VarInit.hasNoSideEffect varInit
