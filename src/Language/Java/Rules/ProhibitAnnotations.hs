module Language.Java.Rules.ProhibitAnnotations where

import Control.Monad (MonadPlus (..))
import Data.List (find)
import Language.Java.AST (extractAnnotations)
import Language.Java.Syntax (Annotation (..), CompilationUnit, Ident (Ident), Name (..))
import RDF (Diagnostic, simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  annotation <- extractAnnotations cUnit
  checkAnnotation annotation path

checkAnnotation :: Annotation -> FilePath -> [Diagnostic]
checkAnnotation annotation path =
  let name = case annotation of
        NormalAnnotation (Name idents) _ -> concatMap (\(Ident str) -> str) idents
        SingleElementAnnotation (Name idents) _ -> concatMap (\(Ident str) -> str) idents
        MarkerAnnotation (Name idents) -> concatMap (\(Ident str) -> str) idents

      whitelist = ["FooBar"]
   in case Data.List.find (== name) whitelist of
        Nothing -> return (simpleDiagnostic ("Prohibited Annotation found: " ++ name) path)
        Just _ -> mzero
