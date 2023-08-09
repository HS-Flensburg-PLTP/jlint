module Language.Java.Rules.CheckNonFinalMethodParameters (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import Language.Java.Syntax.Ident as Ident
import Language.Java.Syntax.VarDecl as VarDecl
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  FormalParam span modifier _ _ varid <- universeBi cUnit
  checkFormalParams modifier varid path span

checkFormalParams :: [Modifier Parsed] -> VarDeclId -> FilePath -> SourceSpan -> [RDF.Diagnostic]
checkFormalParams modifier varid path span =
  if any (eq IgnoreSourceSpan Final) modifier
    then mzero
    else
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.CheckNonFinalMethodParameters"
            (Ident.name (VarDecl.varDeclIdIdent varid) ++ " ist nicht als Final deklariert.")
            span
            path
        )
