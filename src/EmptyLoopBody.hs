{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EmptyLoopBody where

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
    checkStatement (Do Empty _) = return (simpleDiagnostic (msg "A Do-Loop" methodName) path)
    checkStatement (While _ Empty) = return (simpleDiagnostic (msg "A While-Loop" methodName) path)
    checkStatement (BasicFor _ _ _ Empty) = return (simpleDiagnostic (msg "A For-Loop" methodName) path)
    checkStatement (EnhancedFor _ _ _ _ Empty) = return (simpleDiagnostic (msg "A ForEach-Lopp" methodName) path)
    checkStatement _ = mzero

    msg t methodName = t ++ " in function " ++ methodName ++ " has a empty loop body."
