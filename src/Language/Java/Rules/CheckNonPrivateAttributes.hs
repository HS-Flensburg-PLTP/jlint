module Language.Java.Rules.CheckNonPrivateAttributes (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Foldable (toList)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF
import qualified String

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  FieldDecl span modifier _ varDecls <- universeBi cUnit :: [MemberDecl Parsed]
  if any Modifier.isPrivate modifier
    then mzero
    else
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.CheckNonPrivateAttributes"
            [ String.plural (length varDecls) "Das Attribut" "Die Attribute",
              String.enumerate (map (Markdown.code . Ident.name . VarDecl.ident) (toList varDecls)),
              String.plural (length varDecls) "sollte" "sollten",
              "als",
              Markdown.code "private",
              "deklariert sein."
            ]
            span
            path
        )
