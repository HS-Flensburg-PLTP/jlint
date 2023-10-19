module Language.Java.Rules.Evaluation (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mzero
  where
    -- universeBi cUnit >>= checkMethodCall

    checkMethodCall :: MethodInvocation Parsed -> [RDF.Diagnostic]
    checkMethodCall (MethodCall _ _ _ []) = mzero
    checkMethodCall (MethodCall span _ ident args) =
      if all isConstant args
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.Evaluation"
                [ "Die Methode",
                  Markdown.code (prettyPrint ident),
                  "wird nur mit konstanten Argumenten aufgerufen.",
                  "Statt die Methode aufzurufen, kann das Ergebnis des Methodenaufrufs genutzt werden."
                ]
                span
                path
            )
        else mzero
    checkMethodCall _ = mzero

isConstant :: Exp p -> Bool
isConstant (Lit _) = True
isConstant _ = False
