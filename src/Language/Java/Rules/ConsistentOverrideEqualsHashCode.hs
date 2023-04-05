module Language.Java.Rules.ConsistentOverrideEqualsHashCode where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

-- checks if all classes in the filepath consistently override Object equals and hashCode
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

-- extracts all methods from a class that override Object equals and hashCode
extractEqualsHashCode :: ClassDecl -> [MemberDecl]
extractEqualsHashCode classDecl = do
  membDecl <- universeBi classDecl
  extractMatches membDecl
  where
    extractMatches membDecl =
      if isObjectHashCodeMethod membDecl || isObjectHashEqualsMethod membDecl
        then return membDecl
        else mzero

-- checks if method matches profile of Object hashCode
isObjectHashCodeMethod :: MemberDecl -> Bool
isObjectHashCodeMethod (MethodDecl _ [Public] [] (Just (PrimType IntT)) (Ident "hashCode") [] [] _ _) = True
isObjectHashCodeMethod _ = False

-- checks if method matches profile of Object equals
isObjectHashEqualsMethod :: MemberDecl -> Bool
isObjectHashEqualsMethod (MethodDecl _ [Public] [] (Just (PrimType BooleanT)) (Ident "equals") [FormalParam [] (RefType (ClassRefType classType)) False _] [] _ _) =
  isJavaLangObject classType
isObjectHashEqualsMethod _ = False

-- checks if a classType is of Type Object or java.lang.Object
isJavaLangObject :: ClassType -> Bool
isJavaLangObject (ClassType [(Ident "Object", [])]) = True
isJavaLangObject (ClassType [(Ident "java", []), (Ident "lang", []), (Ident "Object", [])]) = True
isJavaLangObject _ = False
