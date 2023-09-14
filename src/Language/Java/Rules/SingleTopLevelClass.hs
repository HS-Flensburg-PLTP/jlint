module Language.Java.Rules.SingleTopLevelClass (check) where

import Control.Monad (MonadPlus (..))
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check (CompilationUnit _ _ (_ : typeDecl : _)) path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.SingleTopLevelClass"
        ["Jede Datei sollte immer nur eine Top-Level-Klasse beinhalten."]
        (sourceSpan typeDecl)
        path
    )
check _ _ = mzero
