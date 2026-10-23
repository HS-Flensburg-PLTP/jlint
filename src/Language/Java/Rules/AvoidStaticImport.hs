module Language.Java.Rules.AvoidStaticImport (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkImport (universeBi cUnit)
  where
    checkImport :: ImportDecl -> Maybe RDF.Diagnostic
    checkImport (ImportDecl sourceSpan allStatic _ _) =
      if allStatic
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidStaticImport"
                [ "Bei statischen Importen ist bei der Verwendung der Methode/Konstante nicht klar, dass die Methode/Konstante in einer anderen Klasse definiert ist. Daher sollte man auf statische Importe verzichten und stattdessen die entsprechenden Klassen importieren."
                ]
                sourceSpan
                path
            )
        else mzero
