module Language.Java.Rules.UsePostIncrementDecrement (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkExpr (universeBi cUnit)
  where
    checkExpr :: Exp Parsed -> Maybe RDF.Diagnostic
    checkExpr (PreIncrement span _) =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.UseIncrementDecrement"
            [ "Anstelle des PreIncrement-Operators",
              Markdown.code "++x",
              "sollte hier der PostIncrement-Operator",
              Markdown.code "x++",
              "verwendet werden."
            ]
            span
            path
        )
    checkExpr (PreDecrement span _) =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.UseIncrementDecrement"
            [ "Anstelle des PreDecrement-Operators",
              Markdown.code "--x",
              "sollte hier der PostDecrement-Operator",
              Markdown.code "x--",
              "verwendet werden."
            ]
            span
            path
        )
    checkExpr _ = mzero
