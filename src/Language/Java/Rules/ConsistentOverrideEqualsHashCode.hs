module Language.Java.Rules.ConsistentOverrideEqualsHashCode where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.Syntax
import qualified RDF
import Control.Monad (MonadPlus(mzero))
import Debug.Trace (trace)


check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  classDecl <- universeBi cUnit
  case length (extractEqualsHashCode classDecl) of
    0 -> mzero
    1 -> return 
      ( RDF.rangeDiagnostic 
      "Language.Java.Rules.ConsistentOverrideEqualsHashCode" 
      "Beide oder keine Ueberrschreiben"
      dummySourceSpan
      path
      )
    2 -> mzero
    _ -> mzero

  

extractClasses :: CompilationUnit -> [ClassDecl]
extractClasses = universeBi

extractEqualsHashCode :: ClassDecl -> [MemberDecl]
extractEqualsHashCode classes = do
  membDecl <- universeBi classes
  extractBoth membDecl
  where
    extractBoth hashCode@(MethodDecl _ [Public] [] (Just (PrimType IntT)) (Ident "hashCode") [] [] _ _) = return hashCode
    extractBoth equals@(MethodDecl _ [Public] [] (Just (PrimType BooleanT)) (Ident "equals") [FormalParam [] (RefType (ClassRefType (ClassType [(Ident "Object",[])]))) False _] [] _ _) = return equals
    extractBoth equals@(MethodDecl _ [Public] [] (Just (PrimType BooleanT)) (Ident "equals") [FormalParam [] (RefType (ClassRefType (ClassType [(Ident "java",[]),(Ident "lang",[]),(Ident "Object",[])]))) False _] [] _ _) = return equals
    extractBoth _ = mzero
