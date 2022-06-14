{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NeedBraces where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  extractDiagnostics methods path

extractDiagnostics :: (String, MethodBody) -> FilePath -> [Diagnostic]
extractDiagnostics (funcName, methodBody) path = do
  stmt <- universeBi methodBody
  extractDiagnostic stmt
  where
    extractDiagnostic (Do (StmtBlock _) _) = mzero
    extractDiagnostic (Do _ _) = return (simpleDiagnostic (msg "A Do-Part" funcName) path)
    extractDiagnostic (While _ (StmtBlock _)) = mzero
    extractDiagnostic (While _ _) = return (simpleDiagnostic (msg "A While-Part" funcName) path)
    extractDiagnostic (BasicFor _ _ _ (StmtBlock _)) = mzero
    extractDiagnostic (BasicFor _ _ _ _) = return (simpleDiagnostic (msg "A For-Part" funcName) path)
    extractDiagnostic (IfThen _ (StmtBlock _)) = mzero
    extractDiagnostic (IfThen _ _) = return (simpleDiagnostic (msg "A IfThen-Part" funcName) path)
    extractDiagnostic (IfThenElse _ (StmtBlock _) (StmtBlock _)) = mzero
    extractDiagnostic (IfThenElse _ _ (StmtBlock _)) = return (simpleDiagnostic (msg "A IfThenElse-Part" funcName) path)
    extractDiagnostic (IfThenElse _ (StmtBlock _) _) = return (simpleDiagnostic (msg "A IfThenElse-Part" funcName) path)
    extractDiagnostic (IfThenElse _ _ _) = return (simpleDiagnostic (msg "A IfThenElse-Part" funcName) path)
    extractDiagnostic _ = mzero

    msg t funcName = t ++ " in function " ++ funcName ++ " contains no braces."
