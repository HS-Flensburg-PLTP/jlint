module Language.Java.Rules.ConsistentOverrideEqualsHashCode where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe
import Language.Java.Syntax
import qualified RDF

-- checks if all classes in the filepath consistently override Object equals and hashCode
check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  classDecl <- universeBi cUnit
  checkConsistentUse (extractEqualsHashCode classDecl)
  where
    checkConsistentUse [] = mzero
    checkConsistentUse [EqualsMethod span] = return (RDF.rangeDiagnostic "Language.Java.Rules.ConsistentOverrideEqualsHashCode" "by overriding 'equals' the class must also override 'hashcode'" span path)
    checkConsistentUse [HashCodeMethod span] = return (RDF.rangeDiagnostic "Language.Java.Rules.ConsistentOverrideEqualsHashCode" "by overriding 'hashCode' the class must also override 'equals'" span path)
    checkConsistentUse (_ : _) = mzero

data EqualsOrHashCode
  = HashCodeMethod SourceSpan
  | EqualsMethod SourceSpan

-- extracts all methods from a class that override Object equals and hashCode
extractEqualsHashCode :: ClassDecl -> [EqualsOrHashCode]
extractEqualsHashCode classDecl = do
  membDecl <- universeBi classDecl
  extractMatches membDecl
  where
    extractMatches membDecl
      | isObjectHashCodeMethod membDecl = return (HashCodeMethod (fromJust (methodDeclSpan membDecl)))
      | isObjectHashEqualsMethod membDecl = return (EqualsMethod (fromJust (methodDeclSpan membDecl)))
      | otherwise = mzero

-- extracts SourceSpan from a MethodDecl
methodDeclSpan :: MemberDecl -> Maybe SourceSpan
methodDeclSpan (MethodDecl span _ _ _ _ _ _ _ _) = Just span
methodDeclSpan _ = Nothing

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
