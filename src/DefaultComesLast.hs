{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultComesLast (check) where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)

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
          checkDefaultComesLast xs (simpleDiagnostic (msg "'default-case'") path : diagnostisList)
        _ : xs ->
          checkDefaultComesLast xs diagnostisList

    msg t =
      t ++ " of 'switch-case' in function " ++ methodName ++ " is not defined last."
