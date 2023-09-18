module Language.Java.Rules.CheckNonPrivateAttributes (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  FieldDecl span modifier _ vardecls <- universeBi cUnit :: [MemberDecl Parsed]
  vardecl <- NonEmpty.toList vardecls
  if any Modifier.isPrivate modifier
    then mzero
    else
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.CheckNonPrivateAttributes"
            [ "Das Attribut",
              Markdown.code (Ident.name (VarDecl.ident vardecl)),
              "sollte als",
              Markdown.code "private",
              "deklariert sein."
            ]
            span
            path
        )
