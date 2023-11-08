{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.DuplicateInConditional (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (childrenBi, universeBi)
import Data.List.Extra (none)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  IfThenElse span _ ifStmt elseStmt <- universeBi cUnit
  checkStmts (blockStmts ifStmt) (blockStmts elseStmt) span path
  where
    blockStmts :: Stmt Parsed -> [BlockStmt Parsed]
    blockStmts (StmtBlock (Block _ blockStmts)) = blockStmts
    blockStmts stmt = [BlockStmt stmt]

checkStmts :: [BlockStmt Parsed] -> [BlockStmt Parsed] -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkStmts [] _ _ _ = mzero
checkStmts _ [] _ _ = mzero
checkStmts [ifBlockStmt] [elseBlockStmt] span path =
  checkFirstStmts ifBlockStmt elseBlockStmt span path
checkStmts ifBlockStmts@(firstIfStmt : _) elseBlockStmts@(firstElseStmt : _) span path =
  checkFirstStmts firstIfStmt firstElseStmt span path
    ++ checkLastStmts (last ifBlockStmts) (last elseBlockStmts) (declaredVars ifBlockStmts) (declaredVars elseBlockStmts) span path
  where
    declaredVars :: [BlockStmt Parsed] -> [Ident]
    declaredVars blockStmts = do
      LocalVars _ _ _ varDecls <- blockStmts
      NonEmpty.toList (NonEmpty.map VarDecl.ident varDecls)

checkFirstStmts :: BlockStmt Parsed -> BlockStmt Parsed -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkFirstStmts ifBlockStmt@(BlockStmt _) elseBlockStmt@(BlockStmt _) span path =
  if eq IgnoreSourceSpan ifBlockStmt elseBlockStmt
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.DuplicateInConditional"
            [ "Die jeweils erste Anweisung im then- und else-Zweig der",
              Markdown.code "if" ++ "-Anweisung",
              "sind gleich. Die Anweisung kann aus beiden Zweigen vor das",
              Markdown.code "if",
              "heraugezogen werden."
            ]
            span
            path
        )
    else mzero
checkFirstStmts _ _ _ _ = mzero

checkLastStmts :: BlockStmt Parsed -> BlockStmt Parsed -> [Ident] -> [Ident] -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkLastStmts ifBlockStmt@(BlockStmt _) elseBlockStmt@(BlockStmt _) ifVars elseVars span path =
  if eq IgnoreSourceSpan ifBlockStmt elseBlockStmt
    && none (varUsedInBlockStmt ifBlockStmt) ifVars
    && none (varUsedInBlockStmt elseBlockStmt) elseVars
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.DuplicateInConditional"
            [ "Die jeweils letzte Anweisung im then- und else-Zweig der",
              Markdown.code "if" ++ "-Anweisung",
              "sind gleich. Die Anweisung kann aus beiden Zweigen hinter das",
              Markdown.code "else",
              "heraugezogen werden."
            ]
            span
            path
        )
    else mzero
checkLastStmts _ _ _ _ _ _ = mzero

varUsedInBlockStmt :: BlockStmt Parsed -> Ident -> Bool
varUsedInBlockStmt blockStmt var =
  any (eq IgnoreSourceSpan var) (variables blockStmt)

variables :: BlockStmt Parsed -> [Ident]
variables blockStmt =
  [ident | ExpName (Name _ (ident :| [])) :: Exp Parsed <- universeBi blockStmt]
