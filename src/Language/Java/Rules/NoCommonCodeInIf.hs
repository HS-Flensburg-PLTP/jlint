{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.NoCommonCodeInIf (check) where

import Control.Monad (guard, mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Extra (none)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF

ruleName :: String
ruleName = "Language.Java.Rules.NoCommonCodeInIf"

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
checkFirstStmts ifBlockStmt@(BlockStmt _) elseBlockStmt@(BlockStmt _) span path = do
  guard (eq IgnoreSourceSpan ifBlockStmt elseBlockStmt)
  return (RDF.rangeDiagnostic ruleName (message "vor") span path)
checkFirstStmts _ _ _ _ = mzero

checkLastStmts :: BlockStmt Parsed -> BlockStmt Parsed -> [Ident] -> [Ident] -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkLastStmts ifBlockStmt@(BlockStmt _) elseBlockStmt@(BlockStmt _) ifVars elseVars span path = do
  guard
    ( eq IgnoreSourceSpan ifBlockStmt elseBlockStmt
        && none (varUsedInBlockStmt ifBlockStmt) ifVars
        && none (varUsedInBlockStmt elseBlockStmt) elseVars
    )
  return (RDF.rangeDiagnostic ruleName (message "hinter") span path)
checkLastStmts _ _ _ _ _ _ = mzero

message :: String -> [String]
message preposition =
  [ "Die jeweils letzten Anweisungen im `then`- und `else`-Zweig der",
    Markdown.code "if" ++ "-Anweisung",
    "sind identisch. Diese identische Anweisung kann aus beiden Zweigen",
    preposition,
    "die",
    Markdown.code "if" ++ "-Anweisung",
    "herausgezogen werden."
  ]

varUsedInBlockStmt :: BlockStmt Parsed -> Ident -> Bool
varUsedInBlockStmt blockStmt var =
  any (eq IgnoreSourceSpan var) (variables blockStmt)

variables :: BlockStmt Parsed -> [Ident]
variables blockStmt =
  [ident | ExpName (Name _ (ident :| [])) :: Exp Parsed <- universeBi blockStmt]
