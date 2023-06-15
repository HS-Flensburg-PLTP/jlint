{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Rules.ReduceScope (check) where

import Control.Monad (MonadPlus (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Extra (none)
import Language.Java.Syntax
import qualified Language.Java.Syntax.BlockStmt as BlockStmt
import qualified Language.Java.Syntax.Exp as Exp
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Stmt as Stmt
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF (Diagnostic, rangeDiagnostic)

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit filePath = do
  blockStmts <- universeBi cUnit
  checkBlockStmts blockStmts
  where
    checkBlockStmts (LocalVars _ _ _ vars : stmts) =
      reduceScopeInBlockStmts (map VarDecl.ident vars) stmts filePath
    checkBlockStmts _ = mzero

reduceScopeInBlockStmts :: [Ident] -> [BlockStmt Parsed] -> FilePath -> [RDF.Diagnostic]
reduceScopeInBlockStmts declVars (BlockStmt span stmt : blockStmts) filePath =
  let varsNotInStmt = filter (\declVar -> none (eq IgnoreSourceSpan declVar) (variables stmt)) declVars
      varsNotInStmts = filter (\declVar -> none (eq IgnoreSourceSpan declVar) (variables blockStmts)) declVars
   in if Stmt.hasNoSideEffect stmt
        then
          if null varsNotInStmt
            then reduceScopeInIf varsNotInStmts stmt filePath
            else
              map
                ( \var ->
                    RDF.rangeDiagnostic
                      "Language.Java.Rules.ReduceScope"
                      (message var)
                      span
                      filePath
                )
                varsNotInStmt
        else reduceScopeInIf varsNotInStmts stmt filePath
reduceScopeInBlockStmts declVars (blockStmt@(LocalVars {}) : blockStmts) filePath =
  let varsNotInStmt = filter (\declVar -> none (eq IgnoreSourceSpan declVar) (variables blockStmt)) declVars
   in {- We could be more precise in checking the side effects of the right-hand sides of the declarations separately. -}
      if BlockStmt.hasNoSideEffect blockStmt && null varsNotInStmt
        then mzero
        else reduceScopeInBlockStmts varsNotInStmt blockStmts filePath
reduceScopeInBlockStmts _ _ _ = mzero

reduceScopeInIf :: [Ident] -> Stmt Parsed -> FilePath -> [RDF.Diagnostic]
reduceScopeInIf declVars (IfThenElse span condition thenStmt elseStmt) path =
  let varsNotInCondition = filter (\declVar -> none (eq IgnoreSourceSpan declVar) (variables condition)) declVars
   in if Exp.hasNoSideEffect condition
        then
          if null varsNotInCondition
            then mzero
            else
              let varsNotInThenOrElse =
                    filter
                      (\var -> none (eq IgnoreSourceSpan var) (variables thenStmt) || none (eq IgnoreSourceSpan var) (variables elseStmt))
                      varsNotInCondition
               in if null varsNotInThenOrElse
                    then mzero
                    else
                      map
                        ( \var ->
                            RDF.rangeDiagnostic
                              "Language.Java.Rules.ReduceScope"
                              (message var)
                              span
                              path
                        )
                        varsNotInThenOrElse
        else mzero
reduceScopeInIf _ _ _ = mzero

message :: Ident -> String
message var =
  "Der Scope der Variable " ++ Markdown.code (Ident.name var) ++ " kann reduziert werden."

variables :: (Data a) => a -> [Ident]
variables parent =
  [ident | Name _ (ident : _) <- universeBi parent]
