module Language.Java.Rules.AvoidStarImports where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax (CompilationUnit, Ident (Ident), ImportDecl (ImportDecl), Name (Name))
import RDF (Diagnostic, simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  importDecl <- universeBi cUnit
  checkImport importDecl
  where
    checkImport (ImportDecl _ (Name idents) allNamesImported) =
      if allNamesImported
        then
          return
            ( simpleDiagnostic
                ( "Keine *-Importe erlaubt: "
                    ++ concatMap
                      (\(Ident str) -> str ++ ".")
                      idents
                )
                path
            )
        else mzero
