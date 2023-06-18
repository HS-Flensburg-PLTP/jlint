{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.ConstantPropagation (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (IfThen _ (BinOp (ExpName outerName) Equal (Lit lit)) thenStmt) = checkInnerStmt path thenStmt outerName lit
    checkStmt (IfThen _ (BinOp (Lit lit) Equal (ExpName outerName)) thenStmt) = checkInnerStmt path thenStmt outerName lit
    checkStmt (IfThenElse _ (BinOp (ExpName outerName) NotEq (Lit lit)) _ elseStmt) = checkInnerStmt path elseStmt outerName lit
    checkStmt (IfThenElse _ (BinOp (Lit lit) NotEq (ExpName outerName)) _ elseStmt) = checkInnerStmt path elseStmt outerName lit
    checkStmt _ = mzero

checkInnerStmt :: FilePath -> Stmt Parsed -> Name -> Literal -> [RDF.Diagnostic]
checkInnerStmt path thenStmt outerName lit = do
  exp :: Exp Parsed <- universeBi thenStmt
  case exp of
    (ExpName innerName@(Name span _)) ->
      if eq IgnoreSourceSpan outerName innerName
        then
          [ RDF.rangeDiagnostic
              "Language.Java.Rules.ConstantPropagation"
              ("Anstatt erneut `" ++ prettyPrint outerName ++ "` zu verwenden, kann hier direkt das Literal `" ++ prettyPrint lit ++ "` verwendet werden.")
              span
              path
          ]
        else mzero
    _ -> mzero
