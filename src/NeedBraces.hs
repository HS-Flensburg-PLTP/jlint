{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NeedBraces where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

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
    checkStatement (Do _ _) = return (methodDiagnostic methodName "A Do-Part contains no braces." path)
    checkStatement (While _ (StmtBlock _)) = mzero
    checkStatement (While _ _) = return (methodDiagnostic methodName "A While-Part contains no braces." path)
    checkStatement (BasicFor _ _ _ (StmtBlock _)) = mzero
    checkStatement (BasicFor {}) = return (methodDiagnostic methodName "A For-Part contains no braces." path)
    checkStatement (EnhancedFor _ _ _ _ (StmtBlock _)) = mzero
    checkStatement (EnhancedFor {}) = return (methodDiagnostic methodName "A ForEach-Part contains no braces." path)
    checkStatement (IfThen _ _ (StmtBlock _)) = mzero
    checkStatement (IfThen _ _ _) = return (methodDiagnostic methodName "A IfThen-Part contains no braces." path)
    checkStatement (IfThenElse _ (StmtBlock _) (StmtBlock _)) = mzero
    checkStatement (IfThenElse _ _ (StmtBlock _)) = return (methodDiagnostic methodName "A IfThenElse-Part contains no braces." path)
    checkStatement (IfThenElse _ (StmtBlock _) _) = return (methodDiagnostic methodName "A IfThenElse-Part contains no braces." path)
    checkStatement (IfThenElse {}) = return (methodDiagnostic methodName "A IfThenElse-Part contains no braces." path)
    checkStatement _ = mzero
