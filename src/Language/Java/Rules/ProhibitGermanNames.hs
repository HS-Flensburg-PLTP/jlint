{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitGermanNames where

import Control.Monad (MonadPlus (..))
import Data.Foldable (toList)
import Data.Function (on)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (pack, replace, unpack)
import Language.Java.SourceSpan (Located (..), SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF
import System.Process (readProcess)
import Text.RE.TDFA (allMatches, matchedText, re, (*=~))

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
  englishWords <- dictionaryLookup EN words
  let definitelyGermanWords = filter (`notElem` englishWords) germanWords
  let groupedDefinitelyGermanWords = NonEmpty.groupBy ((==) `on` sourceSpan) definitelyGermanWords
  return (message path groupedDefinitelyGermanWords)

message :: FilePath -> [NonEmpty IdentPart] -> [RDF.Diagnostic]
message path =
  map
    ( \gWords ->
        RDF.rangeDiagnostic
          "Language.Java.Rules.ProhibitGermanNames"
          ( "Dieser Bezeichner enthält ein oder mehrere folgende deutsche Wörter:" :
            toList (fmap (Markdown.code . part) gWords)
          )
          (sourceSpan (NonEmpty.head gWords))
          path
    )

-- Helping data structures

data IdentPart = IdentPart SourceSpan String

instance Eq IdentPart where
  IdentPart _ part1 == IdentPart _ part2 = part1 == part2

instance Located IdentPart where
  sourceSpan (IdentPart span _) = span

part :: IdentPart -> String
part (IdentPart _ part) = part

splitIdent :: Ident -> [IdentPart]
splitIdent (Ident sourceSpan ident) =
  mapMaybe
    (fmap (IdentPart sourceSpan) . matchedText)
    (allMatches (ident *=~ [re|([a-z]|[A-Z])[a-z]+|([A-Z]{2,})+|]))

data DictLanguage
  = DE
  | EN

resolveDict :: DictLanguage -> String
resolveDict DE = "de_DE"
resolveDict EN = "en_US"

dictionaryLookup :: DictLanguage -> [IdentPart] -> IO [IdentPart]
dictionaryLookup language parts = do
  let strings = fmap part parts
  nonLanguageWords <- aspell language strings
  case language of
    DE -> do
      nonGermanWords <- aspell DE (map replaceUmlauts nonLanguageWords)
      return (filter (\(IdentPart _ part) -> part `notElem` nonGermanWords) parts)
    EN -> return (filter (\(IdentPart _ part) -> part `notElem` nonLanguageWords) parts)
  where
    replaceUmlauts =
      unpack . replace "ue" "ü" . replace "ae" "ä" . replace "oe" "ö" . replace "ss" "ß" . pack

aspell :: DictLanguage -> [String] -> IO [String]
aspell language =
  fmap words . readProcess "aspell" ["list", "-d", resolveDict language, "--ignore-case"] . unwords
