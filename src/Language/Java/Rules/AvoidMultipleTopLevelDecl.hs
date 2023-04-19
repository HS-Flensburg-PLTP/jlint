module Language.Java.Rules.AvoidMultipleTopLevelDecl (check) where

import Control.Monad (MonadPlus (..))
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check (CompilationUnit _ _ (_ : typeDecl : _)) path = [message (typeDeclSourceSpan typeDecl) path]
check _ _ = mzero

typeDeclSourceSpan :: TypeDecl -> SourceSpan
typeDeclSourceSpan (ClassTypeDecl (ClassDecl span _ _ _ _ _ _)) = span
typeDeclSourceSpan (ClassTypeDecl (EnumDecl span _ _ _ _)) = span
typeDeclSourceSpan (InterfaceTypeDecl (InterfaceDecl span _ _ _ _ _ _ _)) = span
typeDeclSourceSpan (ClassTypeDecl (RecordDecl span _ _ _ _ _ _)) = span

message :: SourceSpan -> FilePath -> RDF.Diagnostic
message =
  RDF.rangeDiagnostic
    "Language.Java.Rules.AvoidMultipleTopLevelDecl"
    "Jede Datei sollte immer nur eine Top-Level-Deklaration beinhalten."
