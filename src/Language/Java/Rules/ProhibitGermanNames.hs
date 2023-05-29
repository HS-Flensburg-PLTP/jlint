{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitGermanNames where

import Control.Monad (MonadPlus (mzero))
import Data.Char (toLower)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Graph (path)
import Language.Java.Rules.Dictionary as Dictionary
import Language.Java.SourceSpan
import Language.Java.Syntax
import RDF (Source)
import qualified RDF
import System.Process
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> IO [RDF.Diagnostic]
check cUnit path = do
  idents <- extractIdents cUnit
  let matches = map splitIdent idents
  checkMatches matches path

checkIdentString :: String -> SourceSpan -> FilePath -> IO [RDF.Diagnostic]
checkIdentString identString sourceSpan path =
  if map toLower identString `elem` Dictionary.dict
    then mzero
    else return [RDF.rangeDiagnostic "nonenglish" ("Name: " ++ identString) sourceSpan path]

splitIdent :: Ident -> ([Match String], SourceSpan)
splitIdent (Ident sourceSpan ident) =
  (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]*|]), sourceSpan)

-- Prelude.concatMap
-- ( ( \case
--       Just string -> checkIdentString string sourceSpan path
--       Nothing -> mzero
--   )
--     . matchedText
--   )
--   (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]*|]))

checkMatches :: [([Match String], SourceSpan)] -> FilePath -> IO [RDF.Diagnostic]
checkMatches [] _ = return []
checkMatches (([], _) : matchPairs) path = checkMatches matchPairs path
checkMatches ((match : matches, sourceSpan) : matchPairs) path = do
  result <- checkMatch path (matchedText match) sourceSpan
  results <- checkMatches ((matches, sourceSpan) : matchPairs) path
  return (result ++ results)

checkMatch :: FilePath -> Maybe String -> SourceSpan -> IO [RDF.Diagnostic]
checkMatch _ Nothing _ = mzero
checkMatch path (Just string) sourceSpan = checkIdentString string sourceSpan path

extractIdents :: CompilationUnit -> IO [Ident]
extractIdents cUnit =
  return
    ( do
        ident <- universeBi cUnit
        extractIdent ident
    )

extractIdent ident = ident

--allMatches
{-
([A-Z]|[a-z])[a-z]*{*}

([a-z]|[A-Z])[a-z]*
-}
