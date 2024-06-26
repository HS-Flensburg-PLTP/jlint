module Language.Java.AST where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.Syntax

extractMethods :: CompilationUnit Parsed -> [(String, MethodBody Parsed)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ _ (Ident _ n) _ _ _ b) = return (n, b)
    extractBody _ = mzero

extractAttributes :: CompilationUnit Parsed -> [([String], [Modifier Parsed])]
extractAttributes cUnit = do
  membDecl <- universeBi cUnit
  extractField membDecl
  where
    extractField (FieldDecl _ mods _ vardecl) = return (map (\(VarDecl _ vardeclId _) -> extractVarName vardeclId) (NonEmpty.toList vardecl), mods)
    extractField _ = mzero

extractMethodParameters :: CompilationUnit Parsed -> [(String, [FormalParam Parsed])]
extractMethodParameters cUnit = do
  membDecl <- universeBi cUnit
  extractFormalParam membDecl
  where
    extractFormalParam (MethodDecl _ _ _ _ (Ident _ ident) formalParam _ _ _) = return (ident, formalParam)
    extractFormalParam _ = mzero

extractVarName :: VarDeclId -> String
extractVarName (VarDeclArray _ varDeclId) = extractVarName varDeclId
extractVarName (VarId (Ident _ n)) = n
