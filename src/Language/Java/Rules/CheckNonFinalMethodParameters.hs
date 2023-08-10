module Language.Java.Rules.CheckNonFinalMethodParameters (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import qualified Language.Java.HelperMethods.Ident as Ident
import qualified Language.Java.HelperMethods.Modifier as Modifier
import qualified Language.Java.HelperMethods.VarDecl as VarDecl
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  FormalParam span modifier _ _ varid <- universeBi cUnit :: [FormalParam Parsed]
  if any Modifier.isFinal modifier
    then mzero
    else
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.CheckNonFinalMethodParameters"
            (Ident.name (VarDecl.varDeclIdIdent varid) ++ " ist nicht als Final deklariert.")
            span
            path
        )
