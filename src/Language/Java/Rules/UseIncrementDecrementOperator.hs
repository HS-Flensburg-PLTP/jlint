module Language.Java.Rules.UseIncrementDecrementOperator where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (SourceSpan, dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  exp <- universeBi cUnit
  checkExp exp
  where
    checkExp assign@(Assign span (NameLhs name1) EqualA (BinOp _ (ExpName name2) op (Lit (Int _ 1)))) =
      if eq IgnoreSourceSpan name1 name2
        then message assign (opToPostIncDec op name1) span path
        else mzero
    checkExp assign@(Assign span (NameLhs name1) EqualA (BinOp _ (Lit (Int _ 1)) op (ExpName name2))) =
      if eq IgnoreSourceSpan name1 name2
        then message assign (opToPostIncDec op name1) span path
        else mzero
    checkExp assign@(Assign span (NameLhs name) op (Lit (Int _ 1))) =
      message assign (assignOpToPostIncDec op name) span path
    checkExp _ = mzero

message :: Exp Parsed -> Maybe (Exp Parsed) -> SourceSpan -> FilePath -> [RDF.Diagnostic]
message assign (Just postIncDec) span path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.UseIncrementDecrementOperator"
        ("Anstelle einer Zuweisung " ++ prettyPrint assign ++ " sollte " ++ prettyPrint postIncDec ++ " verwendet werden.")
        span
        path
    )
message _ Nothing _ _ = mzero

opToPostIncDec :: Op -> Name -> Maybe (Exp Parsed)
opToPostIncDec Add name = Just (PostIncrement dummySourceSpan (ExpName name))
opToPostIncDec Sub name = Just (PostDecrement dummySourceSpan (ExpName name))
opToPostIncDec _ _ = Nothing

assignOpToPostIncDec :: AssignOp -> Name -> Maybe (Exp Parsed)
assignOpToPostIncDec AddA name = Just (PostIncrement dummySourceSpan (ExpName name))
assignOpToPostIncDec SubA name = Just (PostDecrement dummySourceSpan (ExpName name))
assignOpToPostIncDec _ _ = Nothing
