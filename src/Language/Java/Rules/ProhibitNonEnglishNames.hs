{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitNonEnglishNames where

import qualified Control.Lens as Lens
import Data.ByteString
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.Text
import Language.Java.SourceSpan
import Language.Java.Syntax
import Network.Wreq
import qualified RDF
import Text.Parsec.Token (GenLanguageDef (identStart))
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

checkIdentString :: p1 -> p2 -> p3 -> [RDF.Diagnostic]
checkIdentString identString sourceSpan path =
  do
    r <- get "https://api.dictionaryapi.dev/api/v2/entries/en/"
    case r Lens.^. responseStatus . statusCode of
      200 -> []
      _ -> []

responseStatCode :: IO ByteString -> RDF.Diagnostic
responseStatCode io = case io of {}

response = get "https://api.dictionaryapi.dev/api/v2/entries/en/"

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
