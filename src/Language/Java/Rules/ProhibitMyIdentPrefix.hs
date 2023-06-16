{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitMyIdentPrefix where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Markdown
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
            ("Der Bezeichner " ++ Markdown.code ident ++ " verwendet das Präfix " ++ Markdown.code "My" ++ ". Dieses Präfix wird in Programmierbeispielen genutzt, wenn man keinen sinnvollen Namen vergeben kann, da das Beispiel minimal gehalten ist. In produktivem Code sollte man diese Art von Bezeichner nicht verwenden.")
            sourceSpan
            path
        )
    else []
