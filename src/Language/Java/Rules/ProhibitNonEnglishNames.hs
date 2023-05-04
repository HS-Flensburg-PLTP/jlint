{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitNonEnglishNames where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  ident <- universeBi cUnit
  splitIdent ident path

checkIdent :: Ident -> FilePath -> [RDF.Diagnostic]
checkIdent ident path =
  return (RDF.rangeDiagnostic "nonenglish" ("Name: " ++ prettyPrint ident) dummySourceSpan path)

splitIdent :: Ident -> FilePath -> [RDF.Diagnostic]
splitIdent (Ident ident) path =
  concatMap
    ( \match ->
        return
          ( RDF.rangeDiagnostic
              "nonenglish"
              ( "Name: "
                  ++ ( \m -> case m of
                         Just a -> a
                         Nothing -> ""
                     )
                    (matchedText match)
              )
              dummySourceSpan
              path
          )
    )
    (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]*|]))

--allMatches
{-
([A-Z]|[a-z])[a-z]*{*}

([a-z]|[A-Z])[a-z]*
-}
