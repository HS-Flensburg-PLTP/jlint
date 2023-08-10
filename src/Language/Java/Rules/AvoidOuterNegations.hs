{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.AvoidOuterNegations where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  exp :: Exp Parsed <- universeBi cUnit
  checkExp exp path
  where
    checkExp (PreNot span (BinOp _ _ op _)) path =
      case invertOp op of
        Just CAnd ->
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidOuterNegations"
                (deMorganMessage op CAnd)
                span
                path
            )
        Just COr ->
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidOuterNegations"
                (deMorganMessage op COr)
                span
                path
            )
        Just invertedOp ->
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidOuterNegations"
                (message op invertedOp)
                span
                path
            )
        Nothing -> mzero
    checkExp _ _ = mzero

message :: Op -> Op -> String
message op invertedOp =
  "Für einen Ausdruck `!(a "
    ++ prettyPrint op
    ++ " b)` kann die Negation nach innen gezogen und `a "
    ++ prettyPrint invertedOp
    ++ " b` angewendet werden. Generell sollten Negationen immer so weit wie möglich nach innen gezogen werden."

deMorganMessage :: Op -> Op -> String
deMorganMessage op invertedOp =
  "Für einen Ausdrcuk `!(a "
    ++ prettyPrint op
    ++ " b)` kann die DeMorgan-Regel angewendet und `!a "
    ++ prettyPrint invertedOp
    ++ " !b` verwendet werden. Generell sollten Negationen immer so weit wie möglich nach innen gezogen werden."

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
