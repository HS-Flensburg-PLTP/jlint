{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SimplifyBooleanReturn where

import AST (extractMethods, extractVarName)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods (extractClassVars cUnit) path

checkStatements :: (String, MethodBody) -> [(String, Bool)] -> FilePath -> [Diagnostic]
checkStatements (methodName, methodBody) classVars path = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (IfThenElse _ a b)
      | isReturnBool a && isReturnBool b = return (methodDiagnostic methodName "A if-then-else part with literal return can be simplified." path)
      | otherwise = mzero
    checkStatement (IfThen _ _ a)
      | isReturnBool a = return (methodDiagnostic methodName "A if-then part with literal return can be simplified." path)
      | otherwise = mzero
    checkStatement _ = mzero

    isReturnBool (Return (Just (Lit (Boolean _)))) = True
    isReturnBool (Return (Just (ExpName (Name varName))))
      | ((\(Ident name) -> name) (head varName), True) `elem` extractMethodVars methodBody =
        True
      | ((\(Ident name) -> name) (head varName), False) `elem` extractMethodVars methodBody =
        False
      | otherwise = ((\(Ident name) -> name) (head varName), True) `elem` classVars
    isReturnBool (StmtBlock (Block [BlockStmt a])) = isReturnBool a
    isReturnBool _ = False

checkIfVarIsBool :: String -> MethodBody -> Bool
checkIfVarIsBool varName methodBody = (varName, True) `elem` extractMethodVars methodBody

extractMethodVars :: MethodBody -> [(String, Bool)]
extractMethodVars methodBody = do
  var <- universeBi methodBody
  checkVars var
  where
    checkVars (LocalVars _ (PrimType BooleanT) varDecl) = concatMap (\(VarDecl varDeclId _) -> [(extractVarName varDeclId, True)]) varDecl
    checkVars (LocalVars _ _ varDecl) = concatMap (\(VarDecl varDeclId _) -> [(extractVarName varDeclId, False)]) varDecl
    checkVars _ = []

extractClassVars :: CompilationUnit -> [(String, Bool)]
extractClassVars cUnit = do
  classDecl <- universeBi cUnit
  checkVars classDecl
  where
    checkVars (FieldDecl _ _ (PrimType BooleanT) varDecl) = concatMap (\(VarDecl varDeclId _) -> [(extractVarName varDeclId, True)]) varDecl
    checkVars (FieldDecl _ _ _ varDecl) = concatMap (\(VarDecl varDeclId _) -> [(extractVarName varDeclId, False)]) varDecl
    checkVars _ = []
