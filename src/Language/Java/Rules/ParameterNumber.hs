module Language.Java.Rules.ParameterNumber (check) where

import Config (ParameterNumberConfig (ParameterNumberConfig))
import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: ParameterNumberConfig -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
check (ParameterNumberConfig (Just max)) cUnit path = doCheck max cUnit path
check (ParameterNumberConfig Nothing) cUnit path = doCheck maxNumber cUnit path

doCheck :: Int -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
doCheck max cUnit path = do
  MethodDecl span _ _ _ _ paramList _ _ _ <- universeBi cUnit
  if length paramList > max
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ParameterNumber"
            ("Es sollten nicht mehr als " ++ show max ++ " Parameter für eine Funktion verwendet werden.")
            span
            path
        )
    else mzero

maxNumber :: Int
maxNumber = 7
