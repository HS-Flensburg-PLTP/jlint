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

check :: CompilationUnit -> FilePath -> IO [RDF.Diagnostic]
check cUnit path = do
  let idents = universeBi cUnit
  let matches = concatMap splitIdent idents
  checkAllMatches matches path

checkAllMatches :: [(String, SourceSpan)] -> FilePath -> IO [RDF.Diagnostic]
checkAllMatches matches path = do
  nonGermanWords <-
    readProcess
      "aspell"
      ["list", "-d", "de_DE", "--ignore-case"]
      (unwords (map fst matches))
  let germanWords =
        concatMap
          ( \(string, sourceSpan) ->
              ([(string, sourceSpan) | string `notElem` lines nonGermanWords])
          )
          matches
  nonEnglishWords <-
    readProcess
      "aspell"
      ["list", "-d", "en_US", "--ignore-case"]
      ( unwords
          ( map
              fst
              germanWords
          )
      )
  let definitelyGermanWords =
        concatMap
          ( \(string, sourceSpan) ->
              ([(string, sourceSpan) | string `elem` lines nonEnglishWords])
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
  calculateSourceSpans
    (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]*|]))
    sourceSpan

calculateSourceSpans :: [Match String] -> SourceSpan -> [(String, SourceSpan)]
calculateSourceSpans [] _ = []
calculateSourceSpans (match : matches) (Location fileStart lineStart colStart, Location fileEnd lineEnd colEnd) = case matchedText match of
  Nothing -> []
  Just string ->
    ( string,
      ( Location fileStart lineStart colStart,
        Location fileEnd lineEnd (colStart + length string - 1)
      )
    ) :
    calculateSourceSpans
      matches
      (Location fileStart lineStart (colStart + length string), Location fileEnd lineEnd colEnd)
