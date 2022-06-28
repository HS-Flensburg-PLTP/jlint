{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultComesLast (check) where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)
2b8f92b8f9a4a8

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkDefaultComesLast methods path

checkDefaultComesLast :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkDefaultComesLast (methodName, methodBody) path = do
  (Switch _ blocks) <- universeBi methodBody
  checkDefaultComesLast blocks mzero
  where
    checkDefaultComesLast blocks diagnostisList =
      case blocks of
        [] ->
          diagnostisList
        (SwitchBlock Default _) : xs@[SwitchBlock _ _] ->
          checkDefaultComesLast xs (methodDiagnostic methodName "Defaultcase in Switch-Case is not defined last" path : diagnostisList)
        _ : xs ->
          checkDefaultComesLast xs diagnostisList

