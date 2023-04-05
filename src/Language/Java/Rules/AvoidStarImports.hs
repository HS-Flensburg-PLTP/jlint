module Language.Java.Rules.AvoidStarImports where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  importDecl <- universeBi cUnit
  checkImport importDecl
  where
    checkImport (ImportDecl _ (Name idents) allNamesImported) =
      if allNamesImported
        then
          return
            ( RDF.simpleDiagnostic
                ( "Keine *-Importe erlaubt: "
                    ++ concatMap
                      (\(Ident str) -> str ++ ".")
                      idents
                )
                path
            )
        else mzero
