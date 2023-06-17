module Language.Java.Rules.ParameterNumber (check, checkWithDefaultValue) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: Maybe Int -> Maybe Int -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
check (Just max) (Just min) cUnit path = doCheck min max cUnit path
check Nothing Nothing cUnit path = doCheck minNumber maxNumber cUnit path
check _ _ cUnit path = doCheck minNumber maxNumber cUnit path

checkWithDefaultValue :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkWithDefaultValue = doCheck minNumber maxNumber

doCheck :: Int -> Int -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
doCheck min max cUnit path = do
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
    else
      if length paramList < min
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.ParameterNumber"
                ("Es sollten nicht weniger als " ++ show min ++ " Parameter für eine Funktion verwendet werden.")
                span
                path
            )
        else mzero

maxNumber :: Int
maxNumber = 7

minNumber :: Int
minNumber = 0
