{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.DuplicateInConditional (check) where

import Control.Monad (MonadPlus (mzero))
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
  checkStmts (extractBlockStmts ifStmt) (extractBlockStmts elseStmt) (gatherTopLevelVars ifStmt) (gatherTopLevelVars elseStmt) span path

checkStmts :: [BlockStmt Parsed] -> [BlockStmt Parsed] -> [Ident] -> [Ident] -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkStmts [] _ _ _ _ _ = mzero
checkStmts _ [] _ _ _ _ = mzero
checkStmts [ifBlockStmt] elseBlockStmts ifVars elseVars span path =
  take
    1
    ( checkFirstStmts ifBlockStmt (head elseBlockStmts) span path
        ++ checkLastStmts ifBlockStmt (last elseBlockStmts) ifVars elseVars span path
    )
checkStmts ifBlockStmts [elseBlockStmt] ifVars elseVars span path =
  take
    1
    ( checkFirstStmts (head ifBlockStmts) elseBlockStmt span path
        ++ checkLastStmts (last ifBlockStmts) elseBlockStmt ifVars elseVars span path
    )
checkStmts ifBlockStmts elseBlockStmts ifVars elseVars span path =
  checkFirstStmts (head ifBlockStmts) (head elseBlockStmts) span path
    ++ checkLastStmts (last ifBlockStmts) (last elseBlockStmts) ifVars elseVars span path

checkFirstStmts :: BlockStmt Parsed -> BlockStmt Parsed -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkFirstStmts ifBlockStmt@(BlockStmt _) elseBlockStmt@(BlockStmt _) span path =
  if eq IgnoreSourceSpan ifBlockStmt elseBlockStmt
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.DuplicateInConditional"
            [ "Die jeweils erste Anweisung im then- und else-Zweig der if-Anweisung sind gleich.",
              "Die Anweisung kann aus beiden Zweigen vor das if heraugezogen werden"
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
              "Die Anweisung kann aus beiden Zweigen hinter das else heraugezogen werden"
            ]
            span
            path
        )
    else mzero
checkLastStmts _ _ _ _ _ _ = mzero

extractBlockStmts :: Stmt Parsed -> [BlockStmt Parsed]
extractBlockStmts (StmtBlock (Block _ blockStmts)) = blockStmts
extractBlockStmts stmt = [BlockStmt stmt]

gatherTopLevelVars :: Stmt Parsed -> [Ident]
gatherTopLevelVars (StmtBlock (Block _ blockStmts)) = do
  LocalVars _ _ _ varDecls :: BlockStmt Parsed <- childrenBi blockStmts
  NonEmpty.toList (NonEmpty.map VarDecl.ident varDecls)
-- [VarDeclId.ident varDeclId | VarDecl _ varDeclId _ :: VarDecl Parsed <- universeBi stmt]
gatherTopLevelVars _ = mzero

varUsedInBlockStmt :: BlockStmt Parsed -> Ident -> Bool
varUsedInBlockStmt blockStmt var =
  any (eq IgnoreSourceSpan var) [ident | ExpName (Name _ (ident :| [])) :: Exp Parsed <- universeBi blockStmt]
