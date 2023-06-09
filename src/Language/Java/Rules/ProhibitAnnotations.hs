module Language.Java.Rules.ProhibitAnnotations where

import Config (Rule (ProhibitAnnotations))
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified RDF

checkWithDefaultValue :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkWithDefaultValue = check annotationWhitelist

check :: [String] -> CompilationUnit -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = do
  annotation <- universeBi cUnit
  checkAnnotation whitelist annotation path

annotationName :: Annotation -> Name
annotationName (NormalAnnotation _ name _) = name
annotationName (SingleElementAnnotation _ name _) = name
annotationName (MarkerAnnotation _ name) = name

annotationWhitelist :: [String]
annotationWhitelist = ["Override"]

checkAnnotation :: [String] -> Annotation -> FilePath -> [RDF.Diagnostic]
checkAnnotation whitelist annotation path =
  let name = prettyPrint (annotationName annotation)
      sourcespan = sourceSpan annotation
   in if name `elem` whitelist
        then mzero
        else return (RDF.rangeDiagnostic "Language.Java.Rules.ProhibitAnnotations" ("Nicht erlaubte Annotation gefunden: " ++ name) sourcespan path)
