{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.NoGermanNames where

import Control.Monad (MonadPlus (..))
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (partition, sortBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Language.Java.SourceSpan (Located (..), SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF
import qualified String
import System.Log.Logger (infoM)
import System.Process (readProcess)
import Text.RE.TDFA (allMatches, matchedText, re, (*=~))

logger :: String
logger = "jlint.NoGermanNames"

check :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
check cUnit path = do
  let varIdents = map (\v -> VarDecl.ident (v :: VarDecl Parsed)) (universeBi cUnit)
  let typeIdents = map typeDeclIdent (universeBi cUnit)
  let methodIdents = mapMaybe methodDeclIdent (universeBi cUnit)
  let lambdaIdents = concatMap lambdaParamIdents (universeBi cUnit)
  let typeParamIdents = map typeParamIdent (universeBi cUnit)
  let enumIdents = map enumIdent (universeBi cUnit)
  let idents = varIdents ++ typeIdents ++ lambdaIdents ++ typeParamIdents ++ enumIdents ++ methodIdents
  checkIdents (sortBy (compare `on` sourceSpan) idents) path

methodDeclIdent :: MemberDecl Parsed -> Maybe Ident
methodDeclIdent (MethodDecl _ _ _ _ ident _ _ _ _) = return ident
methodDeclIdent _ = mzero

typeDeclIdent :: TypeDecl Parsed -> Ident
typeDeclIdent (ClassTypeDecl (ClassDecl _ _ ident _ _ _ _)) = ident
typeDeclIdent (ClassTypeDecl (EnumDecl _ _ ident _ _)) = ident
typeDeclIdent (ClassTypeDecl (RecordDecl _ _ ident _ _ _ _)) = ident
typeDeclIdent (InterfaceTypeDecl (InterfaceDecl _ _ _ ident _ _ _ _)) = ident

lambdaParamIdents :: LambdaParams Parsed -> [Ident]
lambdaParamIdents (LambdaSingleParam ident) = return ident
lambdaParamIdents (LambdaInferredParams idents) = idents
lambdaParamIdents _ = mzero

typeParamIdent :: TypeParam -> Ident
typeParamIdent (TypeParam ident _) = ident

enumIdent :: EnumConstant Parsed -> Ident
enumIdent (EnumConstant ident _ _) = ident

checkIdents :: [Ident] -> FilePath -> IO [RDF.Diagnostic]
checkIdents idents path = do
  let words = concatMap splitIdent idents
  germanWords <- dictionaryLookup DE words
  infoM logger ("Filtered german words: " ++ unwords (map part germanWords))
  englishWords <- dictionaryLookup EN words
  infoM logger ("Filtered english words: " ++ unwords (map part englishWords))
  let definitelyGermanWords = filter (`notElem` englishWords) germanWords
  infoM logger ("Filtered definitely german words: " ++ unwords (map part definitelyGermanWords))
  let groupedDefinitelyGermanWords = NonEmpty.groupBy ((==) `on` sourceSpan) definitelyGermanWords
  return (message path groupedDefinitelyGermanWords)

message :: FilePath -> [NonEmpty IdentPart] -> [RDF.Diagnostic]
message path =
  map
    ( \parts ->
        RDF.rangeDiagnostic
          "Language.Java.Rules.NoGermanNames"
          [ "Der Bezeichner",
            Markdown.code (concatMap part parts),
            "enthält",
            String.plural (length parts) "das deutsche Wort" "die deutschen Wörter",
            String.enumerate (toList (fmap part parts)) ++ ".",
            "Es sollten nur englische Bezeichner verwendet werden."
          ]
          (sourceSpan (NonEmpty.head parts))
          path
    )

-- Helping data structures

data IdentPart = IdentPart SourceSpan String

instance Eq IdentPart where
  IdentPart _ part1 == IdentPart _ part2 =
    map toLower part1 == map toLower part2

instance Located IdentPart where
  sourceSpan (IdentPart span _) = span

part :: IdentPart -> String
part (IdentPart _ part) = part

splitIdent :: Ident -> [IdentPart]
splitIdent ident = splitUnderscoreCaseIdent ident ++ splitCamelCaseIdent ident

splitUnderscoreCaseIdent :: Ident -> [IdentPart]
splitUnderscoreCaseIdent (Ident sourceSpan ident) =
  mapMaybe
    (fmap (IdentPart sourceSpan) . matchedText)
    (allMatches (ident *=~ [re|([A-Z]{2,})+|]))

splitCamelCaseIdent :: Ident -> [IdentPart]
splitCamelCaseIdent (Ident sourceSpan ident) =
  mapMaybe
    (fmap (IdentPart sourceSpan) . matchedText)
    (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]+|]))

data DictLanguage
  = DE
  | EN
  deriving (Eq)

resolveDict :: DictLanguage -> String
resolveDict DE = "de_DE"
resolveDict EN = "en_US"

dictionaryLookup :: DictLanguage -> [IdentPart] -> IO [IdentPart]
dictionaryLookup language parts = do
  let strings = fmap part parts
  nonLanguageWords <- aspell language strings
  case language of
    DE -> do
      let (nonLanguageWordsWithUmlaut, nonLanguageWordsWithoutUmlauts) = partition (any isUmlaut) nonLanguageWords
      nonGermanWordsWithoutUmlauts <-
        fmap (map removeUmlauts) (aspell DE (map (introduceUmlauts . map toLower) nonLanguageWordsWithoutUmlauts))
      let nonGermanWords = nonLanguageWordsWithUmlaut ++ nonGermanWordsWithoutUmlauts
      return (filter (\(IdentPart _ part) -> map toLower part `notElem` nonGermanWords) parts)
    EN -> return (filter (\(IdentPart _ part) -> part `notElem` nonLanguageWords) parts)

aspell :: DictLanguage -> [String] -> IO [String]
aspell language =
  fmap words . readProcess "aspell" ["list", "-d", resolveDict language, "--ignore-case"] . unwords

introduceUmlauts :: String -> String
introduceUmlauts [] = []
introduceUmlauts ('a' : 'e' : rest) = 'ä' : introduceUmlauts rest
introduceUmlauts ('o' : 'e' : rest) = 'ö' : introduceUmlauts rest
introduceUmlauts ('u' : 'e' : rest) = 'ü' : introduceUmlauts rest
introduceUmlauts ('s' : 's' : rest) = 'ß' : introduceUmlauts rest
introduceUmlauts (c : cs) = c : introduceUmlauts cs

removeUmlauts :: String -> String
removeUmlauts [] = []
removeUmlauts ('ä' : rest) = 'a' : 'e' : removeUmlauts rest
removeUmlauts ('ö' : rest) = 'o' : 'e' : removeUmlauts rest
removeUmlauts ('ü' : rest) = 'u' : 'e' : removeUmlauts rest
removeUmlauts ('ß' : rest) = 's' : 's' : removeUmlauts rest
removeUmlauts (c : cs) = c : removeUmlauts cs

isUmlaut :: Char -> Bool
isUmlaut 'ä' = True
isUmlaut 'ö' = True
isUmlaut 'ü' = True
isUmlaut 'ß' = True
isUmlaut _ = False
