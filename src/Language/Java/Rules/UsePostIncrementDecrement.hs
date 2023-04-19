module Language.Java.Rules.UsePostIncrementDecrement (check) where

import Control.Monad
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  expr <- universeBi cUnit
  checkExpr expr
  where
    checkExpr (PreIncrement span _) =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.UseIncrementDecrement"
            ("Anstelle des PreIncrement-Operators " ++ Markdown.code "++x" ++ " sollte hier der PostIncrement-Operator " ++ Markdown.code "x++" ++ " verwendet werden.")
            span
            path
        )
    checkExpr (PreDecrement span _) =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.UseIncrementDecrement"
            ("Anstelle des PreDecrement-Operators " ++ Markdown.code "--x" ++ " sollte hier der PostDecrement-Operator " ++ Markdown.code "x--" ++ " verwendet werden.")
            span
            path
        )
    checkExpr _ = mzero
