{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitNonEnglishNames where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified RDF
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  ident <- universeBi cUnit
  splitIdent ident path

checkIdentString :: String -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkIdentString identString sourceSpan path =
  return (RDF.rangeDiagnostic "nonenglish" ("Name: " ++ identString) sourceSpan path)

splitIdent :: Ident -> FilePath -> [RDF.Diagnostic]
splitIdent (Ident sourceSpan ident) path =
  concatMap
    ( ( \case
          Just string -> checkIdentString string sourceSpan path
          Nothing -> []
      )
        . matchedText
    )
    (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]*|]))

--allMatches
{-
([A-Z]|[a-z])[a-z]*{*}

([a-z]|[A-Z])[a-z]*
-}
