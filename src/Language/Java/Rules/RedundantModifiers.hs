module Language.Java.Rules.RedundantModifiers where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  InterfaceDecl _ InterfaceNormal _ _ _ _ _ body <- universeBi cUnit
  memberDecl <- universeBi body
  mod <- methodModifiers memberDecl
  checkModifier path mod

methodModifiers :: MemberDecl -> [Modifier]
methodModifiers (MethodDecl _ modifiers _ _ _ _ _ _ _) = modifiers
methodModifiers _ = []

checkModifier :: FilePath -> Modifier -> [RDF.Diagnostic]
checkModifier path mod@(Public span) = return (diagnostic path span (show mod))
checkModifier path mod@(Abstract span) = return (diagnostic path span (show mod))
checkModifier _ _ = mzero

diagnostic :: FilePath -> SourceSpan -> String -> RDF.Diagnostic
diagnostic path span name =
  RDF.rangeDiagnostic
    "Language.Java.Rules.RedundantModifier"
    ("Auf den redundanten Modifier " ++ Markdown.code name ++ " sollte in Interfaces verzichtet werden.")
    span
    path
