module Language.Java.Rules.ProhibitAnnotations where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax (Annotation (..), CompilationUnit, Name (..), SourceSpan)
import qualified RDF

check :: [String] -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = do
  annotation <- universeBi cUnit
  checkAnnotation whitelist annotation path

{-whitelist :: [String]
whitelist = ["FooBar"]-}

annotationName :: Annotation -> Name
annotationName annotation = case annotation of
  NormalAnnotation _ name _ -> name
  SingleElementAnnotation _ name _ -> name
  MarkerAnnotation _ name -> name

annotationSourceSpan :: Annotation -> SourceSpan
annotationSourceSpan annotation = case annotation of
  NormalAnnotation sourcespan _ _ -> sourcespan
  SingleElementAnnotation sourcespan _ _ -> sourcespan
  MarkerAnnotation sourcespan _ -> sourcespan

checkAnnotation :: [String] -> Annotation -> FilePath -> [RDF.Diagnostic]
checkAnnotation whitelist annotation path =
  let name = prettyPrint (annotationName annotation)
      sourcespan = annotationSourceSpan annotation
   in if name `elem` whitelist
        then mzero
        else return (RDF.rangeDiagnostic "ProhibitAnnotations" ("Prohibited Annotation found: " ++ name) sourcespan path)
