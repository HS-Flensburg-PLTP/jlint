module Language.Java.Rules.ProhibitAnnotations where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: [String] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = do
  annotation <- universeBi cUnit
  checkAnnotation whitelist annotation path

checkWithDefaultValue :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkWithDefaultValue = check annotationWhitelist

annotationName :: Annotation Parsed -> Name
annotationName (NormalAnnotation _ name _) = name
annotationName (SingleElementAnnotation _ name _) = name
annotationName (MarkerAnnotation _ name) = name

checkAnnotation :: [String] -> Annotation Parsed -> FilePath -> [RDF.Diagnostic]
checkAnnotation whitelist annotation path =
  let name = prettyPrint (annotationName annotation)
      sourcespan = sourceSpan annotation
   in if name `elem` whitelist
        then mzero
        else
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.ProhibitAnnotations"
                ["Die Nutzung der Annotation", name, "ist nicht erlaubt."]
                sourcespan
                path
            )

annotationWhitelist :: [String]
annotationWhitelist = ["Override"]
