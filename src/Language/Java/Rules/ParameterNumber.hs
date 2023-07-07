{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.ParameterNumber (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (fromMaybe)
import Language.Java.Syntax
import qualified RDF

check :: Maybe Int -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check max cUnit path =
  let maxVal = fromMaybe 7 max
   in do
        MethodDecl span _ _ _ _ paramList _ _ _ :: MemberDecl Parsed <- universeBi cUnit
        if length paramList > maxVal
          then
            return
              ( RDF.rangeDiagnostic
                  "Language.Java.Rules.ParameterNumber"
                  ("Es sollten nicht mehr als " ++ show maxVal ++ " Parameter für eine Funktion verwendet werden.")
                  span
                  path
              )
          else mzero
