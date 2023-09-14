module Language.Java.Rules.ConsistentOverrideEqualsHashCode (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (maybeToList)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

-- checks if all classes in the filepath consistently override Object equals and hashCode
check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  classDecl <- universeBi cUnit
  checkConsistentUse (extractEqualsHashCode classDecl)
  where
    checkConsistentUse :: [(Method, Method, SourceSpan)] -> [RDF.Diagnostic]
    checkConsistentUse [] = mzero
    checkConsistentUse [(found, missing, span)] =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ConsistentOverrideEqualsHashCode"
            [ "Wenn die Methode",
              Markdown.code (methodToString found),
              "überschrieben wird, sollte auch die Methode",
              Markdown.code (methodToString missing),
              "überschrieben werden."
            ]
            span
            path
        )
    checkConsistentUse (_ : _) = mzero

data Method
  = Equals
  | HashCode

methodToString :: Method -> String
methodToString Equals = "equals"
methodToString HashCode = "hashCode"

-- extracts all methods from a class that override Object equals and hashCode
extractEqualsHashCode :: ClassDecl Parsed -> [(Method, Method, SourceSpan)]
extractEqualsHashCode classDecl = do
  membDecl <- universeBi classDecl
  maybeToList (filterEqualsAndHashCode membDecl)

-- checks if a MembderDecl overrides equals or hashcode
filterEqualsAndHashCode :: MemberDecl Parsed -> Maybe (Method, Method, SourceSpan)
filterEqualsAndHashCode (MethodDecl span [Public _] [] (Just (PrimType IntT)) (Ident _ "hashCode") [] [] _ _) = Just (HashCode, Equals, span)
filterEqualsAndHashCode (MethodDecl span [Public _] [] (Just (PrimType BooleanT)) (Ident _ "equals") [FormalParam _ [] (RefType (ClassRefType classType)) False _] [] _ _) =
  if isJavaLangObject classType then Just (Equals, HashCode, span) else Nothing
filterEqualsAndHashCode _ = Nothing

-- checks if a classType is of Type Object or java.lang.Object
isJavaLangObject :: ClassType -> Bool
isJavaLangObject (ClassType ((Ident _ "Object", []) :| [])) = True
isJavaLangObject (ClassType ((Ident _ "java", []) :| [(Ident _ "lang", []), (Ident _ "Object", [])])) = True
isJavaLangObject _ = False
