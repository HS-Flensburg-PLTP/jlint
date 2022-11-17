{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Rules.ReduceScope where

import Control.Monad (MonadPlus (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import qualified Language.Java.Purity as Purity
import Language.Java.Syntax
  ( BlockStmt (BlockStmt, LocalVars),
    CompilationUnit,
    Exp (..),
    Ident,
    Name (Name),
    Stmt (..),
  )
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import RDF (Diagnostic, rangeDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks (LocalVars range _ _ vars : BlockStmt stmt : stmts) =
      let declVars = map VarDecl.ident vars
          varsNotInStmt = filter (`notElem` variables stmt) declVars
          varsNotInStmts = filter (`notElem` variables stmts) declVars
       in if Purity.hasNoSideEffect stmt
            then
              if null varsNotInStmt
                then reduceVarsInIf varsNotInStmts stmt path
                else
                  map
                    ( \var ->
                        rangeDiagnostic
                          "Language.Java.Rules.ReduceScope"
                          (message var)
                          range
                          path
                    )
                    varsNotInStmt
            else reduceVarsInIf varsNotInStmts stmt path
    checkBlocks _ = mzero

reduceVarsInIf :: [Ident] -> Stmt -> FilePath -> [Diagnostic]
reduceVarsInIf declVars (IfThenElse range condition thenStmt elseStmt) path =
  let varsNotInCondition = filter (`notElem` variables condition) declVars
   in if Purity.hasNoSideEffectExp condition
        then
          if null varsNotInCondition
            then mzero
            else
              let varsNotInThenOrElse =
                    filter
                      (\var -> var `notElem` variables thenStmt || var `notElem` variables elseStmt)
                      varsNotInCondition
               in if not (null varsNotInThenOrElse)
                    then
                      map
                        ( \var ->
                            rangeDiagnostic
                              "Language.Java.Rules.ReduceScope"
                              (message var)
                              range
                              path
                        )
                        varsNotInThenOrElse
                    else mzero
        else mzero
reduceVarsInIf _ _ _ = mzero

message :: Ident -> String
message var =
  "Der Scope der Variable " ++ Markdown.code (Ident.name var) ++ " kann reduziert werden."

variables :: (Data a) => a -> [Ident]
variables parent = [ident | ExpName (Name idents) <- universeBi parent, ident <- idents]
