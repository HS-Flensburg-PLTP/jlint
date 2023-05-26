{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitNonEnglishNames where

import qualified Control.Lens as Lens
import Control.Monad (MonadPlus (mzero))
import Data.Char (toLower)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Rules.Dictionary as Dictionary
import Language.Java.SourceSpan
import Language.Java.Syntax
import Network.Wreq
import qualified RDF
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  ident <- universeBi cUnit
  splitIdent ident path

{-
https://api.dictionaryapi.dev/api/v2/entries/en/<word>
404 = not found
200 = found + json

return (RDF.rangeDiagnostic "nonenglish" ("Name: " ++ identString) sourceSpan path)
-}

checkIdentString :: String -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkIdentString identString sourceSpan path = if map toLower identString `elem` Dictionary.dict then mzero else return (RDF.rangeDiagnostic "nonenglish" ("Name: " ++ identString) sourceSpan path)

{-
  do
    r <- get "https://api.dictionaryapi.dev/api/v2/entries/en/"
    case r Lens.^. responseStatus . statusCode of
      200 -> mzero
      _ -> mzero

response = get "https://api.dictionaryapi.dev/api/v2/entries/en/"
-}

splitIdent :: Ident -> FilePath -> [RDF.Diagnostic]
splitIdent (Ident sourceSpan ident) path =
  Prelude.concatMap
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
