{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitGermanNames where

import Data.Generics.Uniplate.Data (universeBi)
import Data.List (groupBy)
import Data.Text (pack, replace, unpack)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified Markdown
import qualified RDF
import System.Process
import Text.RE.TDFA

data DictLanguage
  = DE
  | EN

resolveDict :: DictLanguage -> String
resolveDict DE = "de_DE"
resolveDict EN = "en_US"

dictionaryLookup :: DictLanguage -> [String] -> IO [String]
dictionaryLookup language =
  fmap words . readProcess "aspell" ["list", "-d", resolveDict language, "--ignore-case"] . unwords

check :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
check cUnit = checkIdents (universeBi cUnit)

checkIdents :: [Ident] -> FilePath -> IO [RDF.Diagnostic]
checkIdents idents path = do
  let words = concatMap splitIdent idents
  nonGermanWords <- dictionaryLookup DE (map fst words)
  nonGermanWordsWithUmlautCheck <- dictionaryLookup DE (map replaceUmlautsInString nonGermanWords)
  let germanWords =
        filter
          ( \(word, _) ->
              word `notElem` nonGermanWordsWithUmlautCheck
          )
          words
  nonEnglishWords <- dictionaryLookup EN (map fst germanWords)
  let definitelyGermanWords =
        filter
          ( \(word, _) ->
              word `elem` nonEnglishWords
          )
          germanWords
  let groupedDefinitelyGermanWords = groupBy (\(_, sp1) (_, sp2) -> sp1 == sp2) definitelyGermanWords
  return
    ( map
        ( \gWords ->
            RDF.rangeDiagnostic
              "Language.Java.Rules.ProhibitGermanNames"
              ("Dieser Bezeichner enthält ein oder mehrere folgende deutsche Wörter: " ++ unwords (map (Markdown.code . fst) gWords))
              (snd (head gWords))
              path
        )
        groupedDefinitelyGermanWords
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

replaceUmlautsInString :: String -> String
replaceUmlautsInString =
  unpack . replace "ue" "ü" . replace "ae" "ä" . replace "oe" "ö" . replace "ss" "ß" . pack
