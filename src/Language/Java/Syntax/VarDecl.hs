module Language.Java.Syntax.VarDecl
  ( isInitialized,
    hasNoSideEffect,
    ident,
  )
where

import Language.Java.Syntax (Ident, VarDecl (VarDecl), VarDeclId (VarDeclArray, VarId))
import qualified Language.Java.Syntax.VarInit as VarInit

isInitialized :: VarDecl -> Bool
isInitialized (VarDecl _ (Just _)) = True
isInitialized (VarDecl _ Nothing) = False

ident :: VarDecl -> Ident
ident (VarDecl varDeclId _) = varDeclIdIdent varDeclId

varDeclIdIdent :: VarDeclId -> Ident
varDeclIdIdent (VarDeclArray varDeclId) = varDeclIdIdent varDeclId
varDeclIdIdent (VarId ident) = ident

hasNoSideEffect :: VarDecl -> Bool
hasNoSideEffect (VarDecl _ Nothing) = True
hasNoSideEffect (VarDecl _ (Just varInit)) = VarInit.hasNoSideEffect varInit
