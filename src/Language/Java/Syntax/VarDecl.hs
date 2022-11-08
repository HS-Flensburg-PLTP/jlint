module Language.Java.Syntax.VarDecl
  ( isInitialized,
    ident,
  )
where

import Language.Java.Syntax (Ident, VarDecl (VarDecl), VarDeclId (VarDeclArray, VarId))

isInitialized :: VarDecl -> Bool
isInitialized (VarDecl _ (Just _)) = True
isInitialized (VarDecl _ Nothing) = False

ident :: VarDecl -> Ident
ident (VarDecl varDeclId _) = varDeclIdIdent varDeclId

varDeclIdIdent :: VarDeclId -> Ident
varDeclIdIdent (VarDeclArray varDeclId) = varDeclIdIdent varDeclId
varDeclIdIdent (VarId ident) = ident
