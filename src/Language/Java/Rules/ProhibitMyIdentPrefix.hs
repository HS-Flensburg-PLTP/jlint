{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitMyIdentPrefix (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF
import Text.RE.TDFA.String (matched, re, (?=~))

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe (checkIdent path) (universeBi cUnit)

checkIdent :: FilePath -> Ident -> Maybe RDF.Diagnostic
checkIdent path (Ident sourceSpan ident) =
  if matched (ident ?=~ [re|^([Mm]y)|MY|])
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ProhibitMyIdentPrefix"
            [ "Der Bezeichner",
              Markdown.code ident,
              "verwendet das Präfix",
              Markdown.code "My" ++ ".",
              "Dieses Präfix wird in Programmierbeispielen genutzt, wenn man keinen sinnvollen Namen vergeben kann, da das Beispiel minimal gehalten ist. In produktivem Code sollte man diese Art von Bezeichner nicht verwenden."
            ]
            sourceSpan
            path
        )
    else mzero
