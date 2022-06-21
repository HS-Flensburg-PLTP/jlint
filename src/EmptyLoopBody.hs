{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EmptyLoopBody where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic, simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods path

checkStatements :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (Do Empty _) = return (simpleDiagnostic (methodDiagnostic methodName "A Do-Loop has a empty loop body.") path)
    checkStatement (While _ Empty) = return (simpleDiagnostic (methodDiagnostic methodName "A While-Loop has a empty loop body.") path)
    checkStatement (BasicFor _ _ _ Empty) = return (simpleDiagnostic (methodDiagnostic methodName "A For-Loop has a empty loop body.") path)
    checkStatement (EnhancedFor _ _ _ _ Empty) = return (simpleDiagnostic (methodDiagnostic methodName "A ForEach-Lopp has a empty loop body.") path)
    checkStatement _ = mzero
