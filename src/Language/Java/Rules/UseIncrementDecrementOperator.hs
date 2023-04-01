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
    Stmt (BasicFor, ExpStmt, Return),
    dummySourceSpan,
  )
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  getDeclarations stmt
  where
    getDeclarations (ExpStmt span (Assign (NameLhs name1) EqualA (BinOp (ExpName name2) op (Lit (Int 1))))) =
      assignError name1 name2 op span path
    getDeclarations (ExpStmt span (Assign (NameLhs name1) EqualA (BinOp (Lit (Int 1)) op (ExpName name2)))) =
      assignError name1 name2 op span path
    getDeclarations (Return (Just (Assign (NameLhs name1) EqualA (BinOp (ExpName name2) op (Lit (Int 1)))))) =
      assignError name1 name2 op dummySourceSpan path
    getDeclarations (Return (Just (Assign (NameLhs name1) EqualA (BinOp (Lit (Int 1)) op (ExpName name2))))) =
      assignError name1 name2 op dummySourceSpan path
    getDeclarations (ExpStmt span (Assign _ op (Lit (Int 1)))) =
      compoundAssError span path op
    getDeclarations (Return (Just ((Assign _ op (Lit (Int 1)))))) =
      compoundAssError dummySourceSpan path op
    getDeclarations (BasicFor _ _ (Just [Assign (NameLhs name1) EqualA (BinOp (ExpName name2) op (Lit (Int 1)))]) _) =
      assignError name1 name2 op dummySourceSpan path
    getDeclarations (BasicFor _ _ (Just [Assign (NameLhs name1) EqualA (BinOp (Lit (Int 1)) op (ExpName name2))]) _) =
      assignError name1 name2 op dummySourceSpan path
    getDeclarations (BasicFor _ _ (Just [Assign _ op (Lit (Int 1))]) _) =
      compoundAssError dummySourceSpan path op
    getDeclarations _ = mzero

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

getUnaryOp :: Op -> String
getUnaryOp Add = "++"
getUnaryOp Sub = "--"
getUnaryOp _ = ""

getPrettyOp :: Op -> String
getPrettyOp Add = "+"
getPrettyOp Sub = "-"
getPrettyOp _ = ""

getPrettyAssignOp :: AssignOp -> String
getPrettyAssignOp SubA = "-="
getPrettyAssignOp AddA = "+="
getPrettyAssignOp _ = ""

getAssignToUnaryOp :: AssignOp -> String
getAssignToUnaryOp SubA = "--"
getAssignToUnaryOp AddA = "++"
getAssignToUnaryOp _ = ""

assignMessage :: Op -> String
assignMessage op =
  "Anstelle einer Anweisung x = x " ++ getPrettyOp op ++ " 1 sollte x" ++ getUnaryOp op ++ " verwendet werden."

compoundMessage :: AssignOp -> String
compoundMessage op =
  "Anstelle der Verbundzuweisung x " ++ getPrettyAssignOp op ++ " 1 sollte hier der Dekrement Operator " ++ getAssignToUnaryOp op ++ " verwendet werden."
