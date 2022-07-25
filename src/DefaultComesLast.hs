{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultComesLast (check) where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkDefaultComesLast methods path

checkDefaultComesLast :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkDefaultComesLast (methodName, methodBody) path = do
  (Switch _ blocks) <- universeBi methodBody
  checkSwitch blocks mzero
  where
    checkSwitch blocks diagnosticList =
      case blocks of
        [] ->
          diagnosticList
        (SwitchBlock Default _) : xs@[SwitchBlock _ _] ->
          checkSwitch xs (methodDiagnostic methodName "Defaultcase in Switch-Case is not defined last" path : diagnosticList)
        _ : xs ->
          checkSwitch xs diagnosticList
