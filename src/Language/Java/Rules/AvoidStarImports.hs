module Language.Java.Rules.AvoidStarImports where

import Control.Monad (MonadPlus (mzero))
import Language.Java.AST (extractImports)
import Language.Java.Syntax (CompilationUnit, Ident (Ident), ImportDecl (ImportDecl), Name (Name))
import RDF (Diagnostic, simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  importDecl <- extractImports cUnit
  checkImport importDecl path

checkImport :: ImportDecl -> FilePath -> [Diagnostic]
checkImport (ImportDecl _ (Name idents) b2) path =
  if b2
    then
      return
        ( simpleDiagnostic
            ( "No *-Import allowed for "
                ++ concatMap
                  (\(Ident str) -> str ++ ".")
                  idents
            )
            path
        )
    else mzero
