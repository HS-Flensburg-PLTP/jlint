{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Rules.EmptyLoopBody (check) where

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
    checkStatement (Do _ Empty _) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A Do-Loop has a empty loop body." dummySourceSpan path)
    checkStatement (Do _ (StmtBlock (Block [])) _) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A Do-Loop has a empty loop body." dummySourceSpan path)
    checkStatement (While _ _ Empty) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A While-Loop has a empty loop body." dummySourceSpan path)
    checkStatement (While _ _ (StmtBlock (Block []))) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A While-Loop has a empty loop body." dummySourceSpan path)
    checkStatement (BasicFor _ _ _ _ Empty) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A For-Loop has a empty loop body." dummySourceSpan path)
    checkStatement (BasicFor _ _ _ _ (StmtBlock (Block []))) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A For-Loop has a empty loop body." dummySourceSpan path)
    checkStatement (EnhancedFor _ _ _ _ _ Empty) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A ForEach-Loop has a empty loop body." dummySourceSpan path)
    checkStatement (EnhancedFor _ _ _ _ _ (StmtBlock (Block []))) = return (RDF.rangeDiagnostic "Language.Java.Rules.EmptyLoopBody" "A ForEach-Loop has a empty loop body." dummySourceSpan path)
    checkStatement _ = mzero
