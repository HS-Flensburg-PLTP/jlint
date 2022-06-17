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
  checkStatements methods path

checkStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (Do (StmtBlock _) _) = mzero
    checkStatement (Do {}) = return (simpleDiagnostic (msg "A Do-Part" methodName) path)
    checkStatement (While _ (StmtBlock _)) = mzero
    checkStatement (While {}) = return (simpleDiagnostic (msg "A While-Part" methodName) path)
    checkStatement (BasicFor _ _ _ (StmtBlock _)) = mzero
    checkStatement (BasicFor {}) = return (simpleDiagnostic (msg "A For-Part" methodName) path)
    checkStatement (EnhancedFor _ _ _ _ (StmtBlock _)) = mzero
    checkStatement (EnhancedFor {}) = return (simpleDiagnostic (msg "A ForEach-Part" methodName) path)
    checkStatement (IfThen _ (StmtBlock _)) = mzero
    checkStatement (IfThen {}) = return (simpleDiagnostic (msg "A IfThen-Part" methodName) path)
    checkStatement (IfThenElse _ (StmtBlock _) (StmtBlock _)) = mzero
    checkStatement (IfThenElse _ _ (StmtBlock _)) = return (simpleDiagnostic (msg "A IfThenElse-Part" methodName) path)
    checkStatement (IfThenElse _ (StmtBlock _) _) = return (simpleDiagnostic (msg "A IfThenElse-Part" methodName) path)
    checkStatement (IfThenElse {}) = return (simpleDiagnostic (msg "A IfThenElse-Part" methodName) path)
    checkStatement _ = mzero

    msg t methodName = t ++ " in function " ++ methodName ++ " contains no braces."
