module Language.Java.Rules.RedundantModifiers (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  InterfaceDecl _ InterfaceNormal _ _ _ _ _ body <- universeBi cUnit :: [InterfaceDecl Parsed]
  memberDecl <- universeBi body
  mod <- methodModifiers memberDecl
  checkModifier path mod

methodModifiers :: MemberDecl Parsed -> [Modifier Parsed]
methodModifiers (MethodDecl _ modifiers _ _ _ _ _ _ _) = modifiers
methodModifiers _ = []

checkModifier :: FilePath -> Modifier Parsed -> [RDF.Diagnostic]
checkModifier path mod@(Public span) = return (diagnostic path span (show mod))
checkModifier path mod@(Abstract span) = return (diagnostic path span (show mod))
checkModifier _ _ = mzero

diagnostic :: FilePath -> SourceSpan -> String -> RDF.Diagnostic
diagnostic path span name =
  RDF.rangeDiagnostic
    "Language.Java.Rules.RedundantModifier"
    [ "Auf den redundanten Modifikator",
      Markdown.code name,
      "sollte in einem Interface verzichtet werden."
    ]
    span
    path
