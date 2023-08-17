{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitGermanNames where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (groupBy, sortBy)
import Data.List.NonEmpty (toList)
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
check cUnit path = do
  let varDeclIdents = concatMap checkVarDeclId (universeBi cUnit)
  let typeDeclIdents = map checkTypeDecl (universeBi cUnit)
  let annotationIdents = concatMap checkAnnotation (universeBi cUnit)
  let lambdaParamIdents = concatMap checkLambdaParams (universeBi cUnit)
  let typeParamIdents = map (\(TypeParam ident _) -> ident) (universeBi cUnit)
  let enumConstIdents = concatMap checkEnumConstant (universeBi cUnit)
  checkIdents
    ( sortBy
        (\(Ident (Location _ linel _, _) _) (Ident (Location _ liner _, _) _) -> compare linel liner)
        ( varDeclIdents
            ++ typeDeclIdents
            ++ annotationIdents
            ++ lambdaParamIdents
            ++ typeParamIdents
            ++ enumConstIdents
        )
    )
    path

checkVarDeclId :: VarDeclId -> [Ident]
checkVarDeclId (VarId ident) = return ident
checkVarDeclId _ = mzero

checkTypeDecl :: TypeDecl Parsed -> Ident
checkTypeDecl (ClassTypeDecl (ClassDecl _ _ ident _ _ _ _)) = ident
checkTypeDecl (ClassTypeDecl (EnumDecl _ _ ident _ _)) = ident
checkTypeDecl (ClassTypeDecl (RecordDecl _ _ ident _ _ _ _)) = ident
checkTypeDecl (InterfaceTypeDecl (InterfaceDecl _ _ _ ident _ _ _ _)) = ident

checkMemberDecl :: MemberDecl Parsed -> [Ident]
checkMemberDecl (FieldDecl _ _ _ varDecls) = concatMap checkVarDeclId (universeBi varDecls)
checkMemberDecl (MethodDecl _ _ _ _ ident _ _ _ _) = return ident
checkMemberDecl _ = mzero

checkAnnotation :: Annotation Parsed -> [Ident]
checkAnnotation (NormalAnnotation _ (Name _ name) _) = toList name
checkAnnotation (SingleElementAnnotation _ (Name _ name) _) = toList name
checkAnnotation (MarkerAnnotation _ (Name _ name)) = toList name

checkLambdaParams :: LambdaParams Parsed -> [Ident]
checkLambdaParams (LambdaSingleParam ident) = return ident
checkLambdaParams (LambdaInferredParams idents) = idents
checkLambdaParams _ = []

checkEnumConstant :: EnumConstant Parsed -> [Ident]
checkEnumConstant (EnumConstant ident _ _) = return ident

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
