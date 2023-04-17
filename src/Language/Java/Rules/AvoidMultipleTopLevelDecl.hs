module Language.Java.Rules.AvoidMultipleTopLevelDecl (check) where

import Control.Monad (MonadPlus (..))
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  checkComUnit cUnit
  where
    checkComUnit (CompilationUnit _ _ typeDecls) =
      case typeDecls of
        [] -> mzero
        [_] -> mzero
        _ -> message (typeDeclSourceSpan (typeDecls !! 1)) path

typeDeclSourceSpan :: TypeDecl -> SourceSpan
typeDeclSourceSpan (ClassTypeDecl (ClassDecl span _ _ _ _ _ _)) = span
typeDeclSourceSpan (ClassTypeDecl (EnumDecl span _ _ _ _)) = span
typeDeclSourceSpan (InterfaceTypeDecl (InterfaceDecl span _ _ _ _ _ _ _)) = span
typeDeclSourceSpan (ClassTypeDecl (RecordDecl span _ _ _ _ _ _)) = span

message :: SourceSpan -> FilePath -> [RDF.Diagnostic]
message span path =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.AvoidMultipleTopLevelDecl"
      "Jede Datei sollte immer nur eine Top-Level-Deklaration beinhalten."
      span
      path
  ]
