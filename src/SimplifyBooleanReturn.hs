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
    checkStatement (IfThenElse _ a b)
      | isReturnBool a && isReturnBool b = return (methodDiagnostic methodName "A if-then-else part with literal return can be simplified." path)
      | otherwise = mzero
    checkStatement (IfThen _ a)
      | isReturnBool a = return (methodDiagnostic methodName "A if-then part with literal return can be simplified." path)
      | otherwise = mzero
    checkStatement _ = mzero

    isReturnBool (Return (Just (Lit (Boolean _)))) = True
    isReturnBool (StmtBlock (Block [BlockStmt a])) = isReturnBool a
    isReturnBool _ = False
