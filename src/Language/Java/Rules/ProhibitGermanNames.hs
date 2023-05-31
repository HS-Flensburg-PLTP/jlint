{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitGermanNames where

import Control.Monad (MonadPlus (mzero))
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
  checkMatchSourceSpanPairs matches path

checkIdentString :: String -> SourceSpan -> FilePath -> IO [RDF.Diagnostic]
checkIdentString identString sourceSpan path = do
  result <- readProcess "aspell" ["-a", "-d", "de_DE"] identString
  if checkAspellResult result
    then
      return
        [ RDF.rangeDiagnostic
            "Language.Java.Rules.ProhibitGermanNames"
            ("Deutsches Wort gefunden: " ++ identString)
            sourceSpan
            path
        ]
    else return []

checkAspellResult :: String -> Bool
checkAspellResult string =
  any
    ( \x ->
        case matchedText x of
          Nothing -> False
          Just _ -> True
    )
    (allMatches (string *=~ [re|\n(.)\n\n|]))

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

checkMatchSourceSpanPairs :: [(String, SourceSpan)] -> FilePath -> IO [RDF.Diagnostic]
checkMatchSourceSpanPairs [] _ = return []
checkMatchSourceSpanPairs ((string, sourceSpan) : matchPairs) path = do
  result <- checkIdentString string sourceSpan path
  results <- checkMatchSourceSpanPairs matchPairs path
  return (result ++ results)

-- checkMatchPair :: FilePath -> ([Match String], SourceSpan) -> IO [RDF.Diagnostic]
-- checkMatchPair _ ([], _) = return []
-- checkMatchPair path (match : matches, sourceSpan) =
--   case matchedText match of
--     Nothing -> mzero
--     Just string -> do
--       result <- checkIdentString string sourceSpan path
--       results <- checkMatchPair path (matches, sourceSpan)
--       return (result ++ results)
