module AST where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax

extractMethods :: CompilationUnit -> [(String, MethodBody)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ (Ident n) _ _ _ b) = return (n, b)
    extractBody _ = mzero

extractAttributes :: CompilationUnit -> [([String], [Modifier])]
extractAttributes cUnit = do
  membDecl <- universeBi cUnit
  extractField membDecl
  where
    extractField (FieldDecl mods _ vardecl) = return (map (\(VarDecl vardeclId _) -> varId vardeclId) vardecl, mods)
    extractField _ = mzero
    varId :: VarDeclId -> String
    varId (VarDeclArray varDeclId) = varId varDeclId
    varId (VarId (Ident n)) = n
