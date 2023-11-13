module Language.Java.Rules.FinalParameters (check) where

import Control.Applicative (empty)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified Language.Java.Syntax.VarDeclId as VarDeclId
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = universeBi cUnit >>= checkClassDecl
  where
    checkClassDecl :: TypeDecl Parsed -> [RDF.Diagnostic]
    checkClassDecl (ClassTypeDecl decl) = universeBi decl >>= checkFormalParam
    checkClassDecl _ = empty

    checkFormalParam :: FormalParam Parsed -> [RDF.Diagnostic]
    checkFormalParam (FormalParam span modifier _ _ ident) =
      if any Modifier.isFinal modifier
        then empty
        else
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.FinalParameters"
                [ "Der Parameter",
                  Markdown.code (Ident.name (VarDeclId.ident ident)),
                  "sollte als",
                  Markdown.code "final",
                  "deklariert sein.",
                  "Auf diese Weise kann der Parameter in der Methode/im Konstruktor nicht verändert werden."
                ]
                span
                path
            )
