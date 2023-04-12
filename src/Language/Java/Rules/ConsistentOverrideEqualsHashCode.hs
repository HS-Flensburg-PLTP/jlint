module Language.Java.Rules.ConsistentOverrideEqualsHashCode where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (maybeToList)
import Language.Java.Syntax
import qualified RDF

-- checks if all classes in the filepath consistently override Object equals and hashCode
check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  classDecl <- universeBi cUnit
  checkConsistentUse (extractEqualsHashCode classDecl)
  where
    checkConsistentUse [] = mzero
    checkConsistentUse [(msg, span)] = return (RDF.rangeDiagnostic "Language.Java.Rules.ConsistentOverrideEqualsHashCode" msg span path)
    checkConsistentUse (_ : _) = mzero

-- extracts all methods from a class that override Object equals and hashCode
extractEqualsHashCode :: ClassDecl -> [(String, SourceSpan)]
extractEqualsHashCode classDecl = do
  membDecl <- universeBi classDecl
  extractMatches membDecl
  where
    extractMatches membDecl = maybeToList (filterEqualsAndHashCode membDecl)

-- checks if a MembderDecl overrides equals or hashcode
filterEqualsAndHashCode :: MemberDecl -> Maybe (String, SourceSpan)
filterEqualsAndHashCode (MethodDecl span [Public _] [] (Just (PrimType IntT)) (Ident "hashCode") [] [] _ _) = Just ("by overriding 'hashCode' the class must also override 'equals'", span)
filterEqualsAndHashCode (MethodDecl span [Public _] [] (Just (PrimType BooleanT)) (Ident "equals") [FormalParam [] (RefType (ClassRefType classType)) False _] [] _ _) =
  if isJavaLangObject classType then Just ("by overriding 'equals' the class must also override 'hashcode'", span) else Nothing
filterEqualsAndHashCode _ = Nothing

-- checks if a classType is of Type Object or java.lang.Object
isJavaLangObject :: ClassType -> Bool
isJavaLangObject (ClassType [(Ident "Object", [])]) = True
isJavaLangObject (ClassType [(Ident "java", []), (Ident "lang", []), (Ident "Object", [])]) = True
isJavaLangObject _ = False
