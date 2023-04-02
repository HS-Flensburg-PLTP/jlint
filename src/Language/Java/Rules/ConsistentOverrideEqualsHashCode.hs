module Language.Java.Rules.ConsistentOverrideEqualsHashCode where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  classDecl <- universeBi cUnit
  checkConsistentUse (extractEqualsHashCode classDecl)
  where
    checkConsistentUse [] = mzero
    checkConsistentUse [MethodDecl span _ _ _ (Ident "equals") _ _ _ _] = return (RDF.rangeDiagnostic "Language.Java.Rules.ConsistentOverrideEqualsHashCode" "by overriding 'equals' the class must also override 'hashcode'" span path)
    checkConsistentUse [MethodDecl span _ _ _ (Ident "hashCode") _ _ _ _] = return (RDF.rangeDiagnostic "Language.Java.Rules.ConsistentOverrideEqualsHashCode" "by overriding 'hashCode' the class must also override 'equals'" span path)
    checkConsistentUse [memberDecl] =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ConsistentOverrideEqualsHashCode"
            (show memberDecl)
            dummySourceSpan
            path
        )
    checkConsistentUse (_ : _) = mzero

extractEqualsHashCode :: ClassDecl -> [MemberDecl]
extractEqualsHashCode classes = do
  membDecl <- universeBi classes
  extractMatches membDecl
  where
    extractMatches hashCode@(MethodDecl _ [Public] [] (Just (PrimType IntT)) (Ident "hashCode") [] [] _ _) = return hashCode
    extractMatches equals@(MethodDecl _ [Public] [] (Just (PrimType BooleanT)) (Ident "equals") [FormalParam [] (RefType (ClassRefType (ClassType [(Ident "Object", [])]))) False _] [] _ _) = return equals
    extractMatches equals@(MethodDecl _ [Public] [] (Just (PrimType BooleanT)) (Ident "equals") [FormalParam [] (RefType (ClassRefType (ClassType [(Ident "java", []), (Ident "lang", []), (Ident "Object", [])]))) False _] [] _ _) = return equals
    extractMatches _ = mzero
