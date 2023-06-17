{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.ConstantPropagation (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (intercalate)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (IfThen span (BinOp (ExpName outerName) Equal (Lit lit)) thenStmt) = checkInnerStmt span path thenStmt outerName lit "then"
    checkStmt (IfThen span (BinOp (Lit lit) Equal (ExpName outerName)) thenStmt) = checkInnerStmt span path thenStmt outerName lit "then"
    checkStmt (IfThenElse span (BinOp (ExpName outerName) NotEq (Lit lit)) _ elseStmt) = checkInnerStmt span path elseStmt outerName lit "else"
    checkStmt (IfThenElse span (BinOp (Lit lit) NotEq (ExpName outerName)) _ elseStmt) = checkInnerStmt span path elseStmt outerName lit "else"
    checkStmt _ = mzero

checkInnerStmt :: SourceSpan -> FilePath -> Stmt Parsed -> Name -> Literal -> String -> [RDF.Diagnostic]
checkInnerStmt span path thenStmt outerName@(Name _ ident) lit ifcase = do
  exp :: Exp Parsed <- universeBi thenStmt
  case exp of
    (ExpName innerName) ->
      if eq IgnoreSourceSpan outerName innerName
        then
          [ RDF.rangeDiagnostic
              "Language.Java.Rules.ConstantPropagation"
              ("Anstatt im " ++ ifcase ++ "-Fall erneut " ++ intercalate ", " (map Ident.name ident) ++ " zu verwenden, kann hier direkt das Literal " ++ prettyPrint lit ++ " verwendet werden.")
              span
              path
          ]
        else mzero
    _ -> mzero
