{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Rules.EmptyLoopBody where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
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
    checkStatement (Do Empty _) = return (methodDiagnostic methodName "A Do-Loop has a empty loop body." path)
    checkStatement (Do (StmtBlock (Block [])) _) = return (methodDiagnostic methodName "A Do-Loop has a empty loop body." path)
    checkStatement (While _ Empty) = return (methodDiagnostic methodName "A While-Loop has a empty loop body." path)
    checkStatement (While _ (StmtBlock (Block []))) = return (methodDiagnostic methodName "A While-Loop has a empty loop body." path)
    checkStatement (BasicFor _ _ _ Empty) = return (methodDiagnostic methodName "A For-Loop has a empty loop body." path)
    checkStatement (BasicFor _ _ _ (StmtBlock (Block []))) = return (methodDiagnostic methodName "A For-Loop has a empty loop body." path)
    checkStatement (EnhancedFor _ _ _ _ Empty) = return (methodDiagnostic methodName "A ForEach-Loop has a empty loop body." path)
    checkStatement (EnhancedFor _ _ _ _ (StmtBlock (Block []))) = return (methodDiagnostic methodName "A ForEach-Loop has a empty loop body." path)
    checkStatement _ = mzero