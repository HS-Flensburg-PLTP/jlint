{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Rules.DefaultComesLast (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkDefaultComesLast methods path

checkDefaultComesLast :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
checkDefaultComesLast (_, methodBody) path = do
  (Switch _ _ blocks) <- universeBi methodBody
  checkSwitch blocks mzero
  where
    checkSwitch blocks diagnosticList =
      case blocks of
        [] ->
          diagnosticList
        (SwitchBlock Default _) : xs@[SwitchBlock _ _] ->
          checkSwitch
            xs
            (RDF.rangeDiagnostic "Language.Java.Rules.DefaultComesLast" "Defaultcase in Switch-Case is not defined last" dummySourceSpan path : diagnosticList)
        _ : xs ->
          checkSwitch xs diagnosticList
