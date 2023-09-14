{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.SimplifyBooleanReturn (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.AST (extractMethods, extractVarName)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods (extractClassVars cUnit) path

checkStatements :: (String, MethodBody Parsed) -> [(String, Bool)] -> FilePath -> [RDF.Diagnostic]
checkStatements (methodName, methodBody) classVars path = do
  stmt :: Stmt Parsed <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (IfThenElse _ _ a b)
      | isReturnBool a && isReturnBool b =
          return
            ( RDF.rangeDiagnostic
                methodName
                [ "Eine",
                  Markdown.code "if" ++ "-Anweisung",
                  "mit einen expliziten",
                  Markdown.code "return",
                  "sollte vereinfacht werden."
                ]
                dummySourceSpan
                path
            )
      | otherwise = mzero
    checkStatement (IfThen _ _ a)
      | isReturnBool a =
          return
            ( RDF.rangeDiagnostic
                methodName
                [ "Eine",
                  Markdown.code "if" ++ "-Anweisung",
                  "mit einen expliziten",
                  Markdown.code "return",
                  "sollte vereinfacht werden."
                ]
                dummySourceSpan
                path
            )
      | otherwise = mzero
    checkStatement _ = mzero

    isReturnBool (Return _ (Just (Lit (Boolean _ _)))) = True
    isReturnBool (Return _ (Just (ExpName (Name _ varName))))
      | ((\(Ident _ name) -> name) (NonEmpty.head varName), True) `elem` extractMethodVars methodBody =
          True
      | ((\(Ident _ name) -> name) (NonEmpty.head varName), False) `elem` extractMethodVars methodBody =
          False
      | otherwise = ((\(Ident _ name) -> name) (NonEmpty.head varName), True) `elem` classVars
    isReturnBool (StmtBlock (Block _ [BlockStmt a])) = isReturnBool a
    isReturnBool _ = False

extractMethodVars :: MethodBody Parsed -> [(String, Bool)]
extractMethodVars methodBody = do
  var :: BlockStmt Parsed <- universeBi methodBody
  checkVars var
  where
    checkVars (LocalVars _ _ (PrimType BooleanT) varDecl) = concatMap (\(VarDecl _ varDeclId _) -> [(extractVarName varDeclId, True)]) varDecl
    checkVars (LocalVars _ _ _ varDecl) = concatMap (\(VarDecl _ varDeclId _) -> [(extractVarName varDeclId, False)]) varDecl
    checkVars _ = []

extractClassVars :: CompilationUnit Parsed -> [(String, Bool)]
extractClassVars cUnit = do
  classDecl :: MemberDecl Parsed <- universeBi cUnit
  checkVars classDecl
  where
    checkVars (FieldDecl _ _ (PrimType BooleanT) varDecl) = concatMap (\(VarDecl _ varDeclId _) -> [(extractVarName varDeclId, True)]) varDecl
    checkVars (FieldDecl _ _ _ varDecl) = concatMap (\(VarDecl _ varDeclId _) -> [(extractVarName varDeclId, False)]) varDecl
    checkVars _ = []
