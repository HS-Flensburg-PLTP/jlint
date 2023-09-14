module Language.Java.Rules.AvoidStarImport (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  importDecl <- universeBi cUnit
  checkImport importDecl
  where
    checkImport :: ImportDecl -> [RDF.Diagnostic]
    checkImport (ImportDecl sourceSpan _ _ allNamesImported) =
      if allNamesImported
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidStarImport"
                [ "Es sollten keine",
                  Markdown.code "*" ++ "-Importe",
                  "verwendet werden. Diese Importe führen dazu, dass nicht klar ist, welche Klassen importiert werden."
                ]
                sourceSpan
                path
            )
        else mzero
