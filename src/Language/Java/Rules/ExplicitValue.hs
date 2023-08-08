{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.ExplicitValue (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit :: [Stmt Parsed]
  checkStmt stmt
  where
    checkStmt (IfThen _ (BinOp _ (ExpName outerName) Equal (Lit lit)) thenStmt) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThen _ (BinOp _ (Lit lit) Equal (ExpName outerName)) thenStmt) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThen _ (PreNot _ (BinOp _ (ExpName outerName) NotEq (Lit lit))) thenStmt) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThen _ (PreNot _ (BinOp _ (Lit lit) NotEq (ExpName outerName))) thenStmt) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThenElse _ (BinOp _ (ExpName outerName) Equal (Lit lit)) thenStmt _) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThenElse _ (BinOp _ (Lit lit) Equal (ExpName outerName)) thenStmt _) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThenElse _ (BinOp _ (ExpName outerName) NotEq (Lit lit)) _ elseStmt) = checkInnerStmt elseStmt outerName lit path
    checkStmt (IfThenElse _ (BinOp _ (Lit lit) NotEq (ExpName outerName)) _ elseStmt) = checkInnerStmt elseStmt outerName lit path
    checkStmt (IfThenElse _ (PreNot _ (BinOp _ (ExpName outerName) NotEq (Lit lit))) thenStmt _) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThenElse _ (PreNot _ (BinOp _ (Lit lit) NotEq (ExpName outerName))) thenStmt _) = checkInnerStmt thenStmt outerName lit path
    checkStmt (IfThenElse _ (PreNot _ (BinOp _ (ExpName outerName) Equal (Lit lit))) _ elseStmt) = checkInnerStmt elseStmt outerName lit path
    checkStmt (IfThenElse _ (PreNot _ (BinOp _ (Lit lit) Equal (ExpName outerName))) _ elseStmt) = checkInnerStmt elseStmt outerName lit path
    checkStmt _ = mzero

checkInnerStmt :: Stmt Parsed -> Name -> Literal -> FilePath -> [RDF.Diagnostic]
checkInnerStmt stmt outerName lit path = do
  exp <- universeBi stmt :: [Exp Parsed]
  case exp of
    ExpName innerName@(Name span _) ->
      if eq IgnoreSourceSpan outerName innerName
        then
          [ RDF.rangeDiagnostic
              "Language.Java.Rules.ExplicitValue"
              ("Anstatt erneut `" ++ prettyPrint outerName ++ "` zu verwenden, kann hier direkt das Literal `" ++ prettyPrint lit ++ "` verwendet werden.")
              span
              path
          ]
        else mzero
    _ -> mzero
