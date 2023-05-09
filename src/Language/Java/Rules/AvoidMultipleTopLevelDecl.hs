module Language.Java.Rules.AvoidMultipleTopLevelDecl (check) where

import Control.Monad (MonadPlus (..))
import Language.Java.SourceSpan (SourceSpan, sourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check (CompilationUnit _ _ (_ : typeDecl : _)) path = [message (typeDeclSourceSpan typeDecl) path]
check _ _ = mzero

-- Reminder: Use "sourceSpan typeDecl", when implemented in language-java
typeDeclSourceSpan :: TypeDecl -> SourceSpan
typeDeclSourceSpan (ClassTypeDecl ctd) = sourceSpan ctd
typeDeclSourceSpan (InterfaceTypeDecl itd) = sourceSpan itd

message :: SourceSpan -> FilePath -> RDF.Diagnostic
message =
  RDF.rangeDiagnostic
    "Language.Java.Rules.AvoidMultipleTopLevelDecl"
    "Jede Datei sollte immer nur eine Top-Level-Deklaration beinhalten."
