{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NeedBraces where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path =
  let fBlocks = extractMethods cUnit

      search [] = []
      search ((n, b) : xs) = extractStatements b n path ++ search xs
   in search fBlocks

extractMethods :: CompilationUnit -> [(String, MethodBody)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ (Ident n) _ _ _ b) = return (n, b)
    extractBody _ = mzero

extractStatements :: MethodBody -> String -> FilePath -> [Diagnostic]
extractStatements methodBody funcName path = do
  stmt <- universeBi methodBody
  extractStatement stmt
  where
    extractStatement (Do (StmtBlock _) _) = mzero
    extractStatement (Do _ _) = return (simpleDiagnostic (msg "A Do-Part" funcName) path)
    extractStatement (While _ (StmtBlock _)) = mzero
    extractStatement (While _ _) = return (simpleDiagnostic (msg "A While-Part" funcName) path)
    extractStatement (BasicFor _ _ _ (StmtBlock _)) = mzero
    extractStatement (BasicFor _ _ _ _) = return (simpleDiagnostic (msg "A For-Part" funcName) path)
    extractStatement (IfThen _ (StmtBlock _)) = mzero
    extractStatement (IfThen _ _) = return (simpleDiagnostic (msg "A IfThen-Part" funcName) path)
    extractStatement (IfThenElse _ (StmtBlock _) (StmtBlock _)) = mzero
    extractStatement (IfThenElse _ _ (StmtBlock _)) = return (simpleDiagnostic (msg "A IfThenElse-Part" funcName) path)
    extractStatement (IfThenElse _ (StmtBlock _) _) = return (simpleDiagnostic (msg "A IfThenElse-Part" funcName) path)
    extractStatement (IfThenElse _ _ _) = return (simpleDiagnostic (msg "A IfThenElse-Part" funcName) path)
    extractStatement _ = mzero

msg :: String -> String -> String
msg t funcName = t ++ " in function " ++ funcName ++ " contains no braces."
