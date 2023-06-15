module Language.Java.Rules.CheckNonPrivateAttributes (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import Language.Java.Syntax.Ident as Ident
import Language.Java.Syntax.VarDecl as VarDecl
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  FieldDecl span modifier _ varid <- universeBi cUnit
  checkAttributes modifier varid path span

checkAttributes :: [Modifier Parsed] -> [VarDecl Parsed] -> FilePath -> SourceSpan -> [RDF.Diagnostic]
checkAttributes modifier vardecls path span = do
  vardecl <- vardecls
  if any (eq IgnoreSourceSpan Private) modifier
    then mzero
    else
      [ RDF.rangeDiagnostic
          "Language.Java.Rules.CheckNonPrivateAttributes"
          (Ident.name (VarDecl.ident vardecl) ++ " ist nicht als Privat deklariert.")
          span
          path
      ]
