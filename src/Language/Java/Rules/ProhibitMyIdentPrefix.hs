{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitMyIdentPrefix where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF
import Text.RE.TDFA.String (matched, re, (?=~))

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  ident <- universeBi cUnit
  checkIdent ident path

checkIdent :: Ident -> FilePath -> [RDF.Diagnostic]
checkIdent (Ident sourceSpan ident) path =
  if matched (ident ?=~ [re|^([Mm]y)|MY|])
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ProhibitMyIdentPrefix"
            ("Nicht erlaubtes 'My'-Prefix gefunden: " ++ ident)
            sourceSpan
            path
        )
    else []
