module UnnecessaryVariables where

import AST (extractMethods, extractVarName)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax (BlockStmt (LocalVars), CompilationUnit, Exp (..), Ident (..), MethodBody, Name (..), Stmt (..), VarDecl (..))
import RDF (Diagnostic, methodDiagnostic)

checkMethodVars :: CompilationUnit -> FilePath -> [Diagnostic]
checkMethodVars cUnit path = do
  (methodName, methodBody) <- extractMethods cUnit
  checkNecessaryVars (extractMethodVars methodBody) (extractMethodVarUsage methodBody) (extractReturnedVars methodBody) methodName path

extractMethodVars :: MethodBody -> [String]
extractMethodVars methodBody = do
  names <- universeBi methodBody
  extractNames names
  where
    extractNames (LocalVars _ _ varDecls) = map (\(VarDecl varDeclId _) -> extractVarName varDeclId) varDecls
    extractNames _ = mzero

extractMethodVarUsage :: MethodBody -> [String]
extractMethodVarUsage methodBody = do
  usedVariables <- universeBi methodBody
  extractUsedVars usedVariables
  where
    extractUsedVars (Name varList) = map (\(Ident n) -> n) varList

extractReturnedVars :: MethodBody -> [String]
extractReturnedVars methodBody = do
  stmt <- universeBi methodBody
  checkReturn stmt
  where
    checkReturn (Return (Just (ExpName (Name varList)))) = map (\(Ident n) -> n) varList
    checkReturn _ = mzero

checkNecessaryVars :: [String] -> [String] -> [String] -> String -> FilePath -> [Diagnostic]
checkNecessaryVars declaredVars usedVars returnedVars methodName path =
  if checkLengthofFilterUsedVars declaredVars usedVars == 1 && checkLenghtofFilterReturnedVars declaredVars returnedVars <= 1
    then map (\var -> methodDiagnostic methodName ("Variable " ++ var ++ " is unnecessary") path) declaredVars
    else []

checkLenghtofFilterReturnedVars :: [String] -> [String] -> Int
checkLenghtofFilterReturnedVars declaredVars returnedVars =
  length (filter (`elem` returnedVars) declaredVars)

checkLengthofFilterUsedVars :: [String] -> [String] -> Int
checkLengthofFilterUsedVars declaredVars usedVars =
  length (filter (`elem` usedVars) declaredVars)
