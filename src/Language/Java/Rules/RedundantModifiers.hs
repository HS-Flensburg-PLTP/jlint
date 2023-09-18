module Language.Java.Rules.RedundantModifiers (check) where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  InterfaceDecl _ InterfaceNormal _ _ _ _ _ body <- universeBi cUnit :: [InterfaceDecl Parsed]
  MethodDecl _ modifiers _ _ _ _ _ _ _ <- universeBi body
  map (diagnostic path) (filter isRedundant modifiers)

isRedundant :: Modifier Parsed -> Bool
isRedundant (Public _) = True
isRedundant (Abstract _) = True
isRedundant _ = False

diagnostic :: FilePath -> Modifier Parsed -> RDF.Diagnostic
diagnostic path modifier =
  RDF.rangeDiagnostic
    "Language.Java.Rules.RedundantModifier"
    [ "Auf den redundanten Modifikator",
      Markdown.code (prettyPrint modifier),
      "sollte in einem Interface verzichtet werden."
    ]
    (sourceSpan modifier)
    path
