module Language.Java.Rules.ConsistentOverrideEqualsHashCode (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe, maybeToList)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Markdown
import qualified RDF

-- checks if all classes in the filepath consistently override Object equals and hashCode
check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  classDecl <- universeBi cUnit
  maybeToList (checkConsistentUse (extractEqualsHashCode classDecl))
  where
    checkConsistentUse :: [(Method, Method, SourceSpan)] -> Maybe RDF.Diagnostic
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
extractEqualsHashCode classDecl = mapMaybe filterEqualsAndHashCode (universeBi classDecl)

-- checks if a MembderDecl overrides equals or hashcode
filterEqualsAndHashCode :: MemberDecl Parsed -> Maybe (Method, Method, SourceSpan)
filterEqualsAndHashCode (MethodDecl span [Public _] [] (Just (PrimType IntT)) ident [] [] _ _) =
  if Ident.name ident == "hashCode"
    then return (HashCode, Equals, span)
    else mzero
filterEqualsAndHashCode (MethodDecl span [Public _] [] (Just (PrimType BooleanT)) ident params [] _ _) =
  if Ident.name ident == "equals" && takesJavaLangObject params
    then return (Equals, HashCode, span)
    else mzero
filterEqualsAndHashCode _ = mzero

takesJavaLangObject :: [FormalParam Parsed] -> Bool
takesJavaLangObject [FormalParam _ [] (RefType (ClassRefType classType)) False _] =
  isJavaLangObject classType
takesJavaLangObject _ = False

-- checks if a classType is of Type Object or java.lang.Object
isJavaLangObject :: ClassType -> Bool
isJavaLangObject (ClassType ((Ident _ "Object", []) :| [])) = True
isJavaLangObject (ClassType ((Ident _ "java", []) :| [(Ident _ "lang", []), (Ident _ "Object", [])])) = True
isJavaLangObject _ = False
