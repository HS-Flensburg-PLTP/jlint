{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SimplifyBooleanReturn where

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
    checkStatement (IfThenElse _ (StmtBlock _) (StmtBlock _)) = mzero
    checkStatement (IfThenElse _ (Return (Just (Lit (Boolean _)))) (Return (Just (Lit (Boolean _))))) = return (methodDiagnostic methodName "A IfThenElse-Part with boolean return can be simplified." path)
    checkStatement (IfThenElse _ (StmtBlock (Block [BlockStmt (Return (Just (Lit (Boolean _))))])) (StmtBlock (Block [BlockStmt (Return (Just (Lit (Boolean _))))]))) = return (methodDiagnostic methodName "A IfThenElse-Part with boolean return can be simplified." path)
    checkStatement (IfThenElse _ (Return (Just (Lit (Boolean _)))) (StmtBlock (Block [BlockStmt (Return (Just (Lit (Boolean _))))]))) = return (methodDiagnostic methodName "A IfThenElse-Part with boolean return can be simplified." path)
    checkStatement (IfThenElse _ (StmtBlock (Block [BlockStmt (Return (Just (Lit (Boolean _))))])) (Return (Just (Lit (Boolean _))))) = return (methodDiagnostic methodName "A IfThenElse-Part with boolean return can be simplified." path)
    checkStatement (IfThen _ (Return (Just (Lit (Boolean _))))) = return (methodDiagnostic methodName "A IfThen-Part with boolean return cann be simplified." path)
    checkStatement (IfThen _ (StmtBlock (Block [BlockStmt (Return (Just (Lit (Boolean _))))]))) = return (methodDiagnostic methodName "A IfThen-Part with boolean return cann be simplified." path)
    checkStatement _ = mzero
