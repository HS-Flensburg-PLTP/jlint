module Language.Java.Rules.ParameterNumber where

import Control.Monad
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  MethodDecl span _ _ _ _ paramList _ _ _ <- universeBi cUnit
  if length paramList > 5
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ParameterNumber"
            "Es sollten nicht mehr als 5 Parameter für eine Funktion verwendet werden."
            span
            path
        )
    else mzero