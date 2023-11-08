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
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  IfThenElse span _ ifStmt elseStmt <- universeBi cUnit
  checkStmts (blockStmts ifStmt) (blockStmts elseStmt) (gatherTopLevelVars ifStmt) (gatherTopLevelVars elseStmt) span path
  where
    blockStmts :: Stmt Parsed -> [BlockStmt Parsed]
    blockStmts (StmtBlock (Block _ blockStmts)) = blockStmts
    blockStmts stmt = [BlockStmt stmt]

checkStmts :: [BlockStmt Parsed] -> [BlockStmt Parsed] -> [Ident] -> [Ident] -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkStmts [] _ _ _ _ _ = mzero
checkStmts _ [] _ _ _ _ = mzero
checkStmts ifBlockStmts@(firstIfStmt : _) elseBlockStmts@(firstElseStmt : _) ifVars elseVars span path =
  checkFirstStmts firstIfStmt firstElseStmt span path
    ++ checkLastStmts (last ifBlockStmts) (last elseBlockStmts) ifVars elseVars span path

checkFirstStmts :: BlockStmt Parsed -> BlockStmt Parsed -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkFirstStmts ifBlockStmt@(BlockStmt _) elseBlockStmt@(BlockStmt _) span path =
  if eq IgnoreSourceSpan ifBlockStmt elseBlockStmt
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.DuplicateInConditional"
            [ "Die jeweils erste Anweisung im then- und else-Zweig der if-Anweisung sind gleich.",
              "Die Anweisung kann aus beiden Zweigen vor das if heraugezogen werden."
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
            [ "Die jeweils letzte Anweisung im then- und else-Zweig der if-Anweisung sind gleich.",
              "Die Anweisung kann aus beiden Zweigen hinter das else heraugezogen werden."
            ]
            span
            path
        )
    else mzero
checkLastStmts _ _ _ _ _ _ = mzero

gatherTopLevelVars :: Stmt Parsed -> [Ident]
gatherTopLevelVars (StmtBlock (Block _ blockStmts)) = do
  LocalVars _ _ _ varDecls :: BlockStmt Parsed <- childrenBi blockStmts
  NonEmpty.toList (NonEmpty.map VarDecl.ident varDecls)
-- [VarDeclId.ident varDeclId | VarDecl _ varDeclId _ :: VarDecl Parsed <- universeBi stmt]
gatherTopLevelVars _ = mzero

varUsedInBlockStmt :: BlockStmt Parsed -> Ident -> Bool
varUsedInBlockStmt blockStmt var =
  any (eq IgnoreSourceSpan var) [ident | ExpName (Name _ (ident :| [])) :: Exp Parsed <- universeBi blockStmt]
