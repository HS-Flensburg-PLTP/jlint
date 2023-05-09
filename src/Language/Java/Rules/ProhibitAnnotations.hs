module Language.Java.Rules.ProhibitAnnotations where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: [String] -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = do
  annotation <- universeBi cUnit
  checkAnnotation whitelist annotation path

annotationName :: Annotation -> Name
annotationName (NormalAnnotation _ name _) = name
annotationName (SingleElementAnnotation _ name _) = name
annotationName (MarkerAnnotation _ name) = name

annotationSourceSpan :: Annotation -> SourceSpan
annotationSourceSpan (NormalAnnotation sourcespan _ _) = sourcespan
annotationSourceSpan (SingleElementAnnotation sourcespan _ _) = sourcespan
annotationSourceSpan (MarkerAnnotation sourcespan _) = sourcespan

checkAnnotation :: [String] -> Annotation -> FilePath -> [RDF.Diagnostic]
checkAnnotation whitelist annotation path =
  let name = prettyPrint (annotationName annotation)
      sourcespan = annotationSourceSpan annotation
   in if name `elem` whitelist
        then mzero
        else return (RDF.rangeDiagnostic "Language.Java.Rules.ProhibitAnnotations" ("Nicht erlaubte Annotation gefunden: " ++ name) sourcespan path)