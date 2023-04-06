module Language.Java.Rules.ProhibitAnnotations where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (find)
import Language.Java.Syntax (Annotation (..), CompilationUnit, Ident (Ident), Name (..), dummySourceSpan)
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  annotation <- universeBi cUnit
  checkAnnotation annotation path

checkAnnotation :: Annotation -> FilePath -> [RDF.Diagnostic]
checkAnnotation annotation path =
  let name = case annotation of
        NormalAnnotation (Name idents) _ -> concatMap (\(Ident str) -> str) idents
        SingleElementAnnotation (Name idents) _ -> concatMap (\(Ident str) -> str) idents
        MarkerAnnotation (Name idents) -> concatMap (\(Ident str) -> str) idents

      whitelist = ["FooBar"]
   in case Data.List.find (== name) whitelist of
        Nothing -> return (RDF.rangeDiagnostic "ProhibitAnnotations" ("Prohibited Annotation found: " ++ name) dummySourceSpan path)
        Just _ -> mzero
