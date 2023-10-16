module Language.Java.Rules.UseIncrementDecrementOperator (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (maybeToList)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (dummySourceSpan, sourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  exp <- universeBi cUnit
  maybeToList (fmap (message path exp) (alternativeExp exp))
  where
    alternativeExp (Assign _ lhs EqualA (BinOp _ var op (Lit (Int _ 1)))) =
      let lhsExp = lhsToExp lhs
       in if eq IgnoreSourceSpan lhsExp var
            then opToPostIncDec op lhsExp
            else mzero
    alternativeExp (Assign _ lhs EqualA (BinOp _ (Lit (Int _ 1)) op var)) =
      let lhsExp = lhsToExp lhs
       in if eq IgnoreSourceSpan lhsExp var
            then opToPostIncDec op lhsExp
            else mzero
    alternativeExp (Assign _ lhs op (Lit (Int _ 1))) =
      assignOpToPostIncDec op (lhsToExp lhs)
    alternativeExp _ = mzero

lhsToExp :: Lhs p -> Exp p
lhsToExp (NameLhs name) = ExpName name
lhsToExp (FieldLhs fieldAccess) = FieldAccess fieldAccess
lhsToExp (ArrayLhs arrayIndex) = ArrayAccess arrayIndex

message :: FilePath -> Exp Parsed -> Exp Parsed -> RDF.Diagnostic
message path assign postIncDec =
  RDF.rangeDiagnostic
    "Language.Java.Rules.UseIncrementDecrementOperator"
    [ "Anstelle einer Zuweisung",
      Markdown.code (prettyPrint assign),
      "sollte",
      Markdown.code (prettyPrint postIncDec),
      "verwendet werden."
    ]
    (sourceSpan assign)
    path

opToPostIncDec :: Op -> Exp Parsed -> Maybe (Exp Parsed)
opToPostIncDec Add name = Just (PostIncrement dummySourceSpan name)
opToPostIncDec Sub name = Just (PostDecrement dummySourceSpan name)
opToPostIncDec _ _ = Nothing

assignOpToPostIncDec :: AssignOp -> Exp Parsed -> Maybe (Exp Parsed)
assignOpToPostIncDec AddA name = Just (PostIncrement dummySourceSpan name)
assignOpToPostIncDec SubA name = Just (PostDecrement dummySourceSpan name)
assignOpToPostIncDec _ _ = Nothing
