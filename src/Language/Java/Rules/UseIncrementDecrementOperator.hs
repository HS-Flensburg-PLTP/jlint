module Language.Java.Rules.UseIncrementDecrementOperator where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( AssignOp (AddA, EqualA, SubA),
    CompilationUnit,
    Exp (Assign, BinOp, ExpName, Lit),
    Lhs (NameLhs),
    Literal (Int),
    Name,
    Op (Add, Sub),
    SourceSpan,
    dummySourceSpan,
  )
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  exp <- universeBi cUnit
  checkExp exp
  where
    checkExp (Assign (NameLhs name1) EqualA (BinOp (ExpName name2) op (Lit (Int 1)))) =
      assignError name1 name2 op dummySourceSpan path
    checkExp (Assign (NameLhs name1) EqualA (BinOp (Lit (Int 1)) op (ExpName name2))) =
      assignError name1 name2 op dummySourceSpan path
    checkExp (Assign _ op (Lit (Int 1))) =
      compoundAssError dummySourceSpan path op
    checkExp _ = mzero

assignError :: Name -> Name -> Op -> SourceSpan -> FilePath -> [RDF.Diagnostic]
assignError name1 name2 op span path =
  if name1 == name2
    then return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrementOperator" (assignMessage op) span path)
    else mzero

compoundAssError :: SourceSpan -> FilePath -> AssignOp -> [RDF.Diagnostic]
compoundAssError span path op =
  if op == SubA || op == AddA
    then return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrementOperator" (compoundMessage op) span path)
    else mzero

unaryOp :: Op -> String
unaryOp Add = "++"
unaryOp Sub = "--"
unaryOp _ = ""

prettyOp :: Op -> String
prettyOp Add = "+"
prettyOp Sub = "-"
prettyOp _ = ""

prettyAssignOp :: AssignOp -> String
prettyAssignOp SubA = "-="
prettyAssignOp AddA = "+="
prettyAssignOp _ = ""

assignToUnaryOp :: AssignOp -> String
assignToUnaryOp SubA = "--"
assignToUnaryOp AddA = "++"
assignToUnaryOp _ = ""

assignMessage :: Op -> String
assignMessage op =
  "Anstelle einer Anweisung x = x " ++ prettyOp op ++ " 1 sollte x" ++ unaryOp op ++ " verwendet werden."

compoundMessage :: AssignOp -> String
compoundMessage op =
  "Anstelle der Verbundzuweisung x " ++ prettyAssignOp op ++ " 1 sollte hier der Dekrement Operator " ++ assignToUnaryOp op ++ " verwendet werden."
