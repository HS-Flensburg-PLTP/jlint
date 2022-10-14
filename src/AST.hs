module AST where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( CompilationUnit,
    FormalParam,
    Ident (Ident),
    MemberDecl (FieldDecl, MethodDecl),
    MethodBody,
    Modifier,
    VarDecl (VarDecl),
    VarDeclId (..),
  )

extractMethods :: CompilationUnit -> [(String, MethodBody)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ _ (Ident n) _ _ _ b) = return (n, b)
    extractBody _ = mzero

extractAttributes :: CompilationUnit -> [([String], [Modifier])]
extractAttributes cUnit = do
  membDecl <- universeBi cUnit
  extractField membDecl
  where
    extractField (FieldDecl _ mods _ vardecl) = return (map (\(VarDecl vardeclId _) -> extractVarName vardeclId) vardecl, mods)
    extractField _ = mzero

extractMethodParameters :: CompilationUnit -> [(String, [FormalParam])]
extractMethodParameters cUnit = do
  membDecl <- universeBi cUnit
  extractFormalParam membDecl
  where
    extractFormalParam (MethodDecl _ _ _ _ (Ident ident) formalParam _ _ _) = return (ident, formalParam)
    extractFormalParam _ = mzero

extractVarName :: VarDeclId -> String
extractVarName (VarDeclArray varDeclId) = extractVarName varDeclId
extractVarName (VarId (Ident n)) = n
