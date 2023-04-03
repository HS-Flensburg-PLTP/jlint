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
  concatMap (checkModifier path) (methodModifiers memberDecl)

methodModifiers :: MemberDecl -> [Modifier]
methodModifiers (MethodDecl _ modifiers _ _ _ _ _ _ _) = modifiers
methodModifiers _ = []

checkModifier :: FilePath -> Modifier -> [RDF.Diagnostic]
checkModifier path Public =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.RedundantModifier"
        ("Auf den redundanten Modifier " ++ Markdown.code "public" ++ " sollte in Interfaces verzichtet werden.")
        dummySourceSpan
        path
    )
checkModifier path Abstract =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.RedundantModifier"
        ("Auf den redundanten Modifier " ++ Markdown.code "abstract" ++ " sollte in Interfaces verzichtet werden.")
        dummySourceSpan
        path
    )
checkModifier _ _ = mzero
