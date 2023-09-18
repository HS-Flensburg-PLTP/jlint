module Language.Java.Rules.AvoidOuterNegations (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkExp (universeBi cUnit)
  where
    checkExp :: Exp Parsed -> Maybe RDF.Diagnostic
    checkExp (PreNot span (BinOp _ _ op _)) =
      case invertOp op of
        Just invertedOp ->
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidOuterNegations"
                (message op invertedOp)
                span
                path
            )
        Nothing -> mzero
    checkExp _ = mzero

message :: Op -> Op -> [String]
message op invertedOp =
  [ "Für einen Ausdruck",
    Markdown.code ("!(a " ++ prettyPrint op ++ " b)"),
    case op of
      CAnd -> "kann die DeMorgan-Regel angewendet"
      COr -> "kann die DeMorgan-Regel angewendet"
      _ -> "kann die Negation nach innen gezogen",
    "und",
    Markdown.code ("a " ++ prettyPrint invertedOp ++ " b"),
    "verwendet werden. Generell sollten Negationen immer so weit wie möglich nach innen gezogen werden."
  ]

invertOp :: Op -> Maybe Op
invertOp LThan = Just GThanE
invertOp GThan = Just LThanE
invertOp LThanE = Just GThan
invertOp GThanE = Just LThan
invertOp Equal = Just NotEq
invertOp NotEq = Just Equal
invertOp CAnd = Just COr
invertOp COr = Just CAnd
invertOp _ = Nothing
