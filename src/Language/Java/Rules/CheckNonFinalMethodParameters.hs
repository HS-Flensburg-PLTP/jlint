module Language.Java.Rules.CheckNonFinalMethodParameters (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified Language.Java.Syntax.VarDeclId as VarDeclId
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  FormalParam span modifier _ _ ident <- universeBi cUnit :: [FormalParam Parsed]
  if any Modifier.isFinal modifier
    then mzero
    else
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.CheckNonFinalMethodParameters"
            [ "Der Parameter",
              Markdown.code (Ident.name (VarDeclId.ident ident)),
              "sollte als",
              Markdown.code "final",
              "deklariert sein. Auf diese Weise kann der Parameter in der Methode/im Konstruktor nicht verändert werden."
            ]
            span
            path
        )
