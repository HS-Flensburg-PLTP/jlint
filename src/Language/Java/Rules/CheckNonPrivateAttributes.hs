module Language.Java.Rules.CheckNonPrivateAttributes (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import Language.Java.Syntax.Ident as Ident
import Language.Java.Syntax.VarDecl as VarDecl
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  FieldDecl span modifier _ vardecls <- universeBi cUnit
  vardecl <- NonEmpty.toList vardecls
  checkAttributes modifier vardecl path span

checkAttributes :: [Modifier Parsed] -> VarDecl Parsed -> FilePath -> SourceSpan -> [RDF.Diagnostic]
checkAttributes modifier vardecl path span = do
  if any isPrivate modifier
    then mzero
    else
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.CheckNonPrivateAttributes"
            (Ident.name (VarDecl.ident vardecl) ++ " ist nicht als Privat deklariert.")
            span
            path
        )

isPrivate :: Modifier Parsed -> Bool
isPrivate Private = True
isPrivate _ = False
