module Language.Java.Rules.ExplicitValue (check) where

import Control.Monad (mplus, mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (IfThen _ cond thenStmt) =
      filterEqualityWithLiteral cond >>= checkInnerStmt path thenStmt
    checkStmt (IfThenElse _ cond thenStmt elseStmt) =
      (filterEqualityWithLiteral cond >>= checkInnerStmt path thenStmt)
        `mplus` (filterUnqualityWithLiteral cond >>= checkInnerStmt path elseStmt)
    checkStmt _ = mzero

filterEqualityWithLiteral :: Exp Parsed -> [(Name, Literal)]
filterEqualityWithLiteral (BinOp _ (ExpName varName) Equal (Lit lit)) =
  return (varName, lit)
filterEqualityWithLiteral (BinOp _ (Lit lit) Equal (ExpName varName)) =
  return (varName, lit)
filterEqualityWithLiteral (PreNot _ (BinOp _ (ExpName varName) NotEq (Lit lit))) =
  return (varName, lit)
filterEqualityWithLiteral (PreNot _ (BinOp _ (Lit lit) NotEq (ExpName varName))) =
  return (varName, lit)
filterEqualityWithLiteral _ =
  mzero

filterUnqualityWithLiteral :: Exp Parsed -> [(Name, Literal)]
filterUnqualityWithLiteral (BinOp _ (ExpName varName) NotEq (Lit lit)) =
  return (varName, lit)
filterUnqualityWithLiteral (BinOp _ (Lit lit) NotEq (ExpName varName)) =
  return (varName, lit)
filterUnqualityWithLiteral (PreNot _ (BinOp _ (ExpName varName) Equal (Lit lit))) =
  return (varName, lit)
filterUnqualityWithLiteral (PreNot _ (BinOp _ (Lit lit) Equal (ExpName varName))) =
  return (varName, lit)
filterUnqualityWithLiteral _ =
  mzero

checkInnerStmt :: FilePath -> Stmt Parsed -> (Name, Literal) -> [RDF.Diagnostic]
checkInnerStmt path stmt (outerName, lit) = do
  exp <- universeBi stmt :: [Exp Parsed]
  case exp of
    ExpName innerName@(Name span _) ->
      if eq IgnoreSourceSpan outerName innerName
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.ExplicitValue"
                [ "Anstatt erneut",
                  Markdown.code (prettyPrint outerName),
                  "zu verwenden, kann hier direkt das Literal",
                  Markdown.code (prettyPrint lit),
                  "verwendet werden."
                ]
                span
                path
            )
        else mzero
    _ -> mzero
