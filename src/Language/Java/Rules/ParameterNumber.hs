{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.ParameterNumber (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  MethodDecl span _ _ _ _ paramList _ _ _ :: MemberDecl Parsed <- universeBi cUnit
  if length paramList > maxNumber
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ParameterNumber"
            ("Es sollten nicht mehr als" ++ show maxNumber ++ "Parameter für eine Funktion verwendet werden.")
            span
            path
        )
    else mzero

maxNumber :: Int
maxNumber = 7
