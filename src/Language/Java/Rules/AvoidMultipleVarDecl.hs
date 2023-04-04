module Language.Java.Rules.AvoidMultipleVarDecl (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax (BlockStmt (LocalVars), CompilationUnit)
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  blockStmts <- universeBi cUnit
  checkBlockStmts blockStmts
  where
    checkBlockStmts (LocalVars span _ _ varDecls) =
      if length varDecls > 1
        then
          [ RDF.rangeDiagnostic
              "Language.Java.Rules.AvoidMultipleVarDecl"
              "Das Deklarieren mehrerer Variablen in der gleichen Zeile sollte vermieden werden."
              span
              path
          ]
        else mzero
    checkBlockStmts _ = mzero
