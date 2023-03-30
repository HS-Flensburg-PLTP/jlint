module Language.Java.Rules.UseIncrementDecrementOperator where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( AssignOp (AddA, EqualA, SubA),
    CompilationUnit,
    Exp (Assign, BinOp, ExpName, Lit),
    Lhs (NameLhs),
    Literal (Int),
    Op (Add, Sub),
    Stmt (ExpStmt),
  )
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  getDeclarations stmt
  where
    getDeclarations (ExpStmt span (Assign (NameLhs name1) EqualA (BinOp (ExpName name2) op (Lit (Int 1))))) =
      if name1 == name2
        then return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrementOperator" (message op) span path)
        else mzero
    getDeclarations (ExpStmt span (Assign (NameLhs name1) EqualA (BinOp (Lit (Int 1)) op (ExpName name2)))) =
      if name1 == name2
        then return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrementOperator" (message op) span path)
        else mzero
    getDeclarations (ExpStmt span (Assign _ SubA (Lit (Int 1)))) =
      return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrementOperator" "Anstelle der Verbundzuweisung x -= 1 sollte hier der Dekrement Operator x-- verwendet werden." span path)
    getDeclarations (ExpStmt span (Assign _ AddA (Lit (Int 1)))) =
      return (RDF.rangeDiagnostic "Language.Java.Rules.UseIncrementDecrementOperator" "Anstelle der Verbundzuweisung x += 1 sollte hier der Dekrement Operator x++ verwendet werden." span path)
    getDeclarations _ = mzero

getUnaryOp :: Op -> String
getUnaryOp Add = "++"
getUnaryOp Sub = "--"
getUnaryOp _ = ""

getPrettyOp :: Op -> String
getPrettyOp Add = "+"
getPrettyOp Sub = "-"
getPrettyOp _ = ""

message :: Op -> String
message op =
  "Anstelle einer Anweisung x = x " ++ getPrettyOp op ++ " 1 sollte x" ++ getUnaryOp op ++ " verwendet werden."