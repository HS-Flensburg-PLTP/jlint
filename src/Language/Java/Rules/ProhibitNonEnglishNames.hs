{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitNonEnglishNames where

import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.Text
import Language.Java.SourceSpan
import Language.Java.Syntax
import Network.HTTP.Req
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
checkIdentString identString sourceSpan path =
  ( \case
      200 -> []
      _ -> return (RDF.rangeDiagnostic "nonenglish" ("Name: " ++ identString) sourceSpan path)
  )
    ( responseStatusCode
        ( req
            GET
            (https (Data.Text.pack "api.dictionaryapi.dev/api/v2/entries/en/<word>") /: Data.Text.pack "get")
            NoReqBody
            jsonResponse
            mempty
        )
    )

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
