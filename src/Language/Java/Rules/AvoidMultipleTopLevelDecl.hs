module Language.Java.Rules.AvoidMultipleTopLevelDecl (check) where

import Control.Monad (MonadPlus (..))
import Language.Java.SourceSpan (SourceSpan, sourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check (CompilationUnit _ _ (_ : typeDecl : _)) path = [message (sourceSpan typeDecl) path]
check _ _ = mzero

message :: SourceSpan -> FilePath -> RDF.Diagnostic
message =
  RDF.rangeDiagnostic
    "Language.Java.Rules.AvoidMultipleTopLevelDecl"
    "Jede Datei sollte immer nur eine Top-Level-Deklaration beinhalten."
