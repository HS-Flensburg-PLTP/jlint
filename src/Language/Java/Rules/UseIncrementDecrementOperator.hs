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
    alternativeExp (Assign _ (NameLhs name1) EqualA (BinOp _ (ExpName name2) op (Lit (Int _ 1)))) =
      if eq IgnoreSourceSpan name1 name2
        then opToPostIncDec op name1
        else mzero
    alternativeExp (Assign _ (NameLhs name1) EqualA (BinOp _ (Lit (Int _ 1)) op (ExpName name2))) =
      if eq IgnoreSourceSpan name1 name2
        then opToPostIncDec op name1
        else mzero
    alternativeExp (Assign _ (NameLhs name) op (Lit (Int _ 1))) =
      assignOpToPostIncDec op name
    alternativeExp _ = mzero

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

opToPostIncDec :: Op -> Name -> Maybe (Exp Parsed)
opToPostIncDec Add name = Just (PostIncrement dummySourceSpan (ExpName name))
opToPostIncDec Sub name = Just (PostDecrement dummySourceSpan (ExpName name))
opToPostIncDec _ _ = Nothing

assignOpToPostIncDec :: AssignOp -> Name -> Maybe (Exp Parsed)
assignOpToPostIncDec AddA name = Just (PostIncrement dummySourceSpan (ExpName name))
assignOpToPostIncDec SubA name = Just (PostDecrement dummySourceSpan (ExpName name))
assignOpToPostIncDec _ _ = Nothing
