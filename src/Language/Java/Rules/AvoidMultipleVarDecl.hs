module Language.Java.Rules.AvoidMultipleVarDecl (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkBlockStmt (universeBi cUnit)
  where
    checkBlockStmt :: BlockStmt Parsed -> Maybe RDF.Diagnostic
    checkBlockStmt (LocalVars span _ _ varDecls) =
      if length varDecls > 1
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidMultipleVarDecl"
                ["Das Deklarieren mehrerer Variablen in der gleichen Zeile sollte vermieden werden."]
                span
                path
            )
        else mzero
    checkBlockStmt _ = mzero
