{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Rules.NeedBraces (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods path

checkStatements :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
checkStatements (_, methodBody) path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (Do _ (StmtBlock _) _) = mzero
    checkStatement (Do {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A Do-Part contains no braces." dummySourceSpan path)
    checkStatement (While _ _ (StmtBlock _)) = mzero
    checkStatement (While {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A While-Part contains no braces." dummySourceSpan path)
    checkStatement (BasicFor _ _ _ _ (StmtBlock _)) = mzero
    checkStatement (BasicFor {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A For-Part contains no braces." dummySourceSpan path)
    checkStatement (EnhancedFor _ _ _ _ _ (StmtBlock _)) = mzero
    checkStatement (EnhancedFor {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A ForEach-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThen _ _ (StmtBlock _)) = mzero
    checkStatement (IfThen {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThen-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThenElse _ _ (StmtBlock _) (StmtBlock _)) = mzero
    checkStatement (IfThenElse _ _ _ (StmtBlock _)) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThenElse-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThenElse _ _ (StmtBlock _) _) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThenElse-Part contains no braces." dummySourceSpan path)
    checkStatement (IfThenElse {}) = return (RDF.rangeDiagnostic "Language.Java.Rules.NeedBraces" "A IfThenElse-Part contains no braces." dummySourceSpan path)
    checkStatement _ = mzero
