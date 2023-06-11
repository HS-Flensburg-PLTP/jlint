module Language.Java.Rules.PullInNegations where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  exp <- universeBi cUnit
  checkExp exp path
  where
    checkExp (PreNot (BinOp _ COr _)) path =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.PullInNegations"
            "Für einen Ausdruck `!(a || b)` kann die DeMorgan-Regel angewendet und `!a && !b` verwendet werden. Generell sollten Negation immer so weit wie möglich nach innen gezogen werden."
            dummySourceSpan
            path
        )
    checkExp (PreNot (BinOp _ CAnd _)) path =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.PullInNegations"
            "Für einen Ausdruck `!(a && b)` kann die DeMorgan-Regel angewendet und `!a || !b` verwendet werden. Generell sollten Negation immer so weit wie möglich nach innen gezogen werden."
            dummySourceSpan
            path
        )
    checkExp (PreNot (BinOp _ op _)) path =
      case invertOp op of
        Just invertedOp ->
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.PullInNegations"
                (message op invertedOp)
                dummySourceSpan
                path
            )
        Nothing -> mzero
    checkExp _ _ = mzero

message :: Op -> Op -> String
message op invertedOp =
  "Für einen Ausdruck `!(a "
    ++ prettyOp op
    ++ " b)` kann die Negation nach innen gezogen und `a "
    ++ prettyOp invertedOp
    ++ " b` angewendet werden. Generell sollten Negation immer so weit wie möglich nach innen gezogen werden."

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

prettyOp :: Op -> String
prettyOp LThan = "<"
prettyOp GThan = ">"
prettyOp LThanE = "<="
prettyOp GThanE = ">="
prettyOp Equal = "=="
prettyOp NotEq = "!="
prettyOp CAnd = "&&"
prettyOp COr = "||"
prettyOp _ = ""
