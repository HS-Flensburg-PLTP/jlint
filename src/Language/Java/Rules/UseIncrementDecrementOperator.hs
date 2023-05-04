module Language.Java.Rules.UseIncrementDecrementOperator where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (SourceSpan, dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  exp <- universeBi cUnit
  checkExp exp
  where
    checkExp assign@(Assign span (NameLhs name1) EqualA (BinOp (ExpName name2) op (Lit (Int 1)))) =
      if name1 == name2
        then message assign (opToPostIncDec op name1) span path
        else mzero
    checkExp assign@(Assign span (NameLhs name1) EqualA (BinOp (Lit (Int 1)) op (ExpName name2))) =
      if name1 == name2
        then message assign (opToPostIncDec op name1) span path
        else mzero
    checkExp assign@(Assign span (NameLhs name) op (Lit (Int 1))) =
      message assign (assignOpToPostIncDec op name) span path
    checkExp _ = mzero

message :: Exp -> Maybe Exp -> SourceSpan -> FilePath -> [RDF.Diagnostic]
message assign (Just postIncDec) span path =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.UseIncrementDecrementOperator"
        ("Anstelle einer Zuweisung " ++ prettyPrint assign ++ " sollte " ++ prettyPrint postIncDec ++ " verwendet werden.")
        span
        path
    )
message _ Nothing _ _ = mzero

opToPostIncDec :: Op -> Name -> Maybe Exp
opToPostIncDec Add name = Just (PostIncrement dummySourceSpan (ExpName name))
opToPostIncDec Sub name = Just (PostDecrement dummySourceSpan (ExpName name))
opToPostIncDec _ _ = Nothing

assignOpToPostIncDec :: AssignOp -> Name -> Maybe Exp
assignOpToPostIncDec AddA name = Just (PostIncrement dummySourceSpan (ExpName name))
assignOpToPostIncDec SubA name = Just (PostDecrement dummySourceSpan (ExpName name))
assignOpToPostIncDec _ _ = Nothing
