module Language.Java.Rules.ParameterNumber (check, checkWithDefaultValue) where

import Config
import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: Rule -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
check (ParameterNumber (Just max) (Just min)) cUnit path = doCheck max cUnit path
check (ParameterNumber Nothing Nothing) cUnit path = doCheck maxNumber cUnit path
check _ cUnit path = doCheck maxNumber cUnit path

checkWithDefaultValue :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkWithDefaultValue = doCheck maxNumber

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
