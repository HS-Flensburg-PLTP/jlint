module Language.Java.Rules.ProhibitNonEnglishNames where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  ident <- universeBi cUnit
  checkIdent ident path

checkIdent :: Ident -> FilePath -> [RDF.Diagnostic]
checkIdent ident path = return (RDF.rangeDiagnostic "nonenglish" ("Name: " ++ prettyPrint ident) dummySourceSpan path)
