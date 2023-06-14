module Language.Java.Syntax.VarDecl
  ( isInitialized,
    hasNoSideEffect,
    ident,
    varDeclIdIdent,
  )
where

import Language.Java.Syntax (Ident, VarDecl (VarDecl), VarDeclId (VarDeclArray, VarId))
import qualified Language.Java.Syntax.VarInit as VarInit

isInitialized :: VarDecl -> Bool
isInitialized (VarDecl _ _ (Just _)) = True
isInitialized (VarDecl _ _ Nothing) = False

ident :: VarDecl -> Ident
ident (VarDecl _ varDeclId _) = varDeclIdIdent varDeclId

varDeclIdIdent :: VarDeclId -> Ident
varDeclIdIdent (VarDeclArray _ varDeclId) = varDeclIdIdent varDeclId
varDeclIdIdent (VarId ident) = ident

hasNoSideEffect :: VarDecl -> Bool
hasNoSideEffect (VarDecl _ _ Nothing) = True
hasNoSideEffect (VarDecl _ _ (Just varInit)) = VarInit.hasNoSideEffect varInit
