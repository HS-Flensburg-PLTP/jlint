{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.UseJavaArrayTypeStyle (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  checkVarDecl cUnit path ++ checkParam cUnit path

checkVarDecl :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkVarDecl cUnit path = do
  VarDecl _ varDeclId _ :: VarDecl Parsed <- universeBi cUnit
  checkVarDeclId varDeclId path

checkParam :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkParam cUnit path = do
  FormalParam _ _ _ _ varDeclId :: FormalParam Parsed <- universeBi cUnit
  checkVarDeclId varDeclId path

checkVarDeclId :: VarDeclId -> FilePath -> [RDF.Diagnostic]
checkVarDeclId (VarDeclArray span _) path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.UseJavaArrayTypeStyle"
        ( "Array-Typen sollten im Java-Stil und nicht im C-Stil definiert werden."
            ++ "Die Arrayklammern `[]` gehören also hinter den Typ und nicht hinter den Namen der Variable."
        )
        span
        path
    )
checkVarDeclId _ _ =
  mzero
