module Language.Java.Rules.AvoidStarImport where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  importDecl <- universeBi cUnit
  checkImport importDecl
  where
    checkImport (ImportDecl _ name allNamesImported) =
      if allNamesImported
        then
          return
            ( RDF.rangeDiagnostic
                "NoStarImports"
                ( "Keine *-Importe erlaubt: "
                    ++ prettyPrint name
                )
                dummySourceSpan
                path
            )
        else mzero
