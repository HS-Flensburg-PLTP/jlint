{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.SimplifyBooleanReturn (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods, extractVarName)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
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
      | isReturnBool a && isReturnBool b = return (RDF.rangeDiagnostic methodName "A if-then-else part with literal return can be simplified." dummySourceSpan path)
      | otherwise = mzero
    checkStatement (IfThen _ _ a)
      | isReturnBool a = return (RDF.rangeDiagnostic methodName "A if-then part with literal return can be simplified." dummySourceSpan path)
      | otherwise = mzero
    checkStatement _ = mzero

    isReturnBool (Return _ (Just (Lit _ (Boolean _)))) = True
    isReturnBool (Return _ (Just (ExpName (Name _ varName))))
      | ((\(Ident _ name) -> name) (head varName), True) `elem` extractMethodVars methodBody =
          True
      | ((\(Ident _ name) -> name) (head varName), False) `elem` extractMethodVars methodBody =
          False
      | otherwise = ((\(Ident _ name) -> name) (head varName), True) `elem` classVars
    isReturnBool (StmtBlock (Block [BlockStmt _ a])) = isReturnBool a
    isReturnBool _ = False

checkIfVarIsBool :: String -> MethodBody Parsed -> Bool
checkIfVarIsBool varName methodBody = (varName, True) `elem` extractMethodVars methodBody

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
