module Language.Java.Rules.AvoidMultipleTopLevelDecl (check) where

import Control.Monad (MonadPlus (..))
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  checkComUnit cUnit
  where
    checkComUnit (CompilationUnit _ _ typeDecls) =
      if length typeDecls > 1
        then case typeDecls !! 1 of
          (ClassTypeDecl (ClassDecl span _ _ _ _ _ _)) -> message span path
          (ClassTypeDecl (EnumDecl span _ _ _ _)) -> message span path
          (InterfaceTypeDecl (InterfaceDecl span _ _ _ _ _ _ _)) -> message span path
          (ClassTypeDecl (RecordDecl span _ _ _ _ _ _)) -> message span path
        else mzero

message :: SourceSpan -> FilePath -> [RDF.Diagnostic]
message span path =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.AvoidMultipleTopLevelDecl"
      "Jede Datei sollte immer nur eine Top-Level-Deklaration beinhalten."
      span
      path
  ]
