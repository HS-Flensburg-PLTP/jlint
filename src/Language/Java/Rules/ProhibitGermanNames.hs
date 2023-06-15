{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitGermanNames where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified RDF
import System.Process
import Text.RE.TDFA.String

data DictLanguage
  = DE
  | EN

resolveDict :: DictLanguage -> String
resolveDict DE = "de_DE"
resolveDict EN = "en_US"

dictionaryLookup :: DictLanguage -> String -> IO String
dictionaryLookup language =
  readProcess
    "aspell"
    ["list", "-d", resolveDict language, "--ignore-case"]

check :: CompilationUnit -> FilePath -> IO [RDF.Diagnostic]
check cUnit path = do
  let idents = universeBi cUnit
  let matches = concatMap splitIdent idents
  checkAllMatches matches path

checkAllMatches :: [(String, SourceSpan)] -> FilePath -> IO [RDF.Diagnostic]
checkAllMatches matches path = do
  nonGermanWords <-
    dictionaryLookup
      DE
      (unwords (map fst matches))
  let germanWords =
        filter
          ( \(word, _) ->
              word `notElem` lines nonGermanWords
          )
          matches
  nonEnglishWords <-
    dictionaryLookup
      EN
      ( unwords
          ( map
              fst
              germanWords
          )
      )
  let definitelyGermanWords =
        filter
          ( \(word, _) ->
              word `elem` lines nonEnglishWords
          )
          germanWords
  return
    ( map
        ( \(string, sourceSpan) ->
            RDF.rangeDiagnostic
              "Language.Java.Rules.ProhibitGermanNames"
              ("Deutsches Wort gefunden: " ++ string)
              sourceSpan
              path
        )
        definitelyGermanWords
    )

splitIdent :: Ident -> [(String, SourceSpan)]
splitIdent (Ident sourceSpan ident) =
  concatMap
    ( \match -> case matchedText match of
        Nothing -> []
        Just string ->
          [(string, sourceSpan)]
    )
    (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]+|([A-Z]{2,})+|]))
