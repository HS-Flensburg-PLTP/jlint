module Language.Java.Rules.ProhibitAnnotations where

import Language.Java.AST (extractAnnotations)
import Language.Java.Syntax (Annotation (..), CompilationUnit, Ident (Ident), Name (..))
import RDF (Diagnostic, simpleDiagnostic)

-- TODO: Erlaubte Annotations
check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  annotation <- extractAnnotations cUnit
  checkAnnotation annotation path

-- TODO: Erlaubte Annotations
checkAnnotation :: Annotation -> FilePath -> [Diagnostic]
checkAnnotation annotation path = case annotation of
  NormalAnnotation (Name idents) _ -> return (simpleDiagnostic ("normal annotation gefunden " ++ concatMap (\(Ident str) -> str) idents) path)
  SingleElementAnnotation (Name idents) _ -> return (simpleDiagnostic ("single ele annotation gefunden " ++ concatMap (\(Ident str) -> str) idents) path)
  MarkerAnnotation (Name idents) -> return (simpleDiagnostic ("marker annotation gefunden " ++ concatMap (\(Ident str) -> str) idents) path)

{-
    NormalAnnotation

    annName :: Name
    annKV :: [(Ident, ElementValue)]

SingleElementAnnotation

    annName :: Name
    annValue :: ElementValue

MarkerAnnotation

    annName :: Name

    -}