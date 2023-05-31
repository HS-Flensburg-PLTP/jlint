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
  let matches = map splitIdent idents
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

splitIdent :: Ident -> ([Match String], SourceSpan)
splitIdent (Ident sourceSpan ident) =
  (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]*|]), sourceSpan)

checkMatchSourceSpanPairs :: [([Match String], SourceSpan)] -> FilePath -> IO [RDF.Diagnostic]
checkMatchSourceSpanPairs [] _ = return []
checkMatchSourceSpanPairs (matchPair : matchPairs) path = do
  result <- checkMatches path matchPair
  results <- checkMatchSourceSpanPairs matchPairs path
  return (result ++ results)

checkMatches :: FilePath -> ([Match String], SourceSpan) -> IO [RDF.Diagnostic]
checkMatches _ ([], _) = return []
checkMatches path (match : matches, sourceSpan) =
  case matchedText match of
    Nothing -> mzero
    Just string -> do
      result <- checkIdentString string sourceSpan path
      results <- checkMatches path (matches, sourceSpan)
      return (result ++ results)
