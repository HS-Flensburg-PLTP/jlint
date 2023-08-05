module Language.Java.Rules.MethodInvNumber where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified RDF

check :: String -> String -> Int -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check called limited maxInv cUnit path = do
  memberDecl <- universeBi cUnit :: [MemberDecl Parsed]
  checkMemberDecl memberDecl
  where
    checkMemberDecl (MethodDecl span _ _ _ ident _ _ _ (MethodBody (Just (Block blockStmts)))) =
      if eq ident called
        then checkBlockStmts blockStmts called limited maxInv span path
        else mzero
    checkMemberDecl _ = mzero

checkBlockStmts :: [BlockStmt Parsed] -> String -> String -> Int -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkBlockStmts blockStmts called limited maxInv span = mzero

expToMethodInv :: [Exp Parsed] -> [MethodInvocation Parsed]
expToMethodInv =
  map
    ( \(MethodInv inv@(MethodCall {})) -> inv
    )

blockStmtToExp :: [BlockStmt Parsed] -> [Exp Parsed]
