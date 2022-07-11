module UnusedLocalVariable where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic, simpleDiagnostic)

checkMethodVars :: CompilationUnit -> FilePath -> [Diagnostic]
checkMethodVars cUnit path = do
  (methodName, methodBody) <- extractMethods cUnit
  checkIfMethodVarsAreUsed (extractMethodVariables methodBody) (extractMethodVariableUsages methodBody) methodName path

extractMethodVariables :: MethodBody -> [String]
extractMethodVariables methodBody = do
  names <- universeBi methodBody
  extractNames names
  where
    extractNames (LocalVars _ _ varDecls) = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecls
    extractNames _ = mzero

extractMethodVariableUsages :: MethodBody -> [String]
extractMethodVariableUsages methodBody = do
  usedVariables <- universeBi methodBody
  extractUsedVariables usedVariables
  where
    extractUsedVariables (Name varList) = map (\(Ident n) -> n) varList

checkIfMethodVarsAreUsed :: [String] -> [String] -> String -> FilePath -> [Diagnostic]
checkIfMethodVarsAreUsed declaredVars usedVars methodName path =
  declaredVars
    & filter (`notElem` usedVars)
    & map (\var -> methodDiagnostic methodName ("Variable " ++ var ++ " is declared but never used.") path)

checkClassVars :: CompilationUnit -> FilePath -> [Diagnostic]
checkClassVars cUnit path =
  concatMap
    ( \var ->
        if checkClassVarUsages (extractMethods cUnit) var
          then mzero
          else [simpleDiagnostic ("LocalVariable " ++ var ++ " is declared but never used.") path]
    )
    (extractClassVars cUnit)

checkClassVarUsages :: [(String, MethodBody)] -> String -> Bool
checkClassVarUsages methods var =
  methods
    & any
      ( \(_, body) ->
          checkClassVarUsageInMethod var (extractMethodVariables body) (extractMethodVariableUsages body)
      )

extractClassVars :: CompilationUnit -> [String]
extractClassVars cUnit = do
  variables <- universeBi cUnit
  extractVars variables
  where
    extractVars (FieldDecl _ _ varDecls) = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecls
    extractVars _ = mzero

checkClassVarUsageInMethod :: String -> [String] -> [String] -> Bool
checkClassVarUsageInMethod var methodVars methodVarUsages =
  notElem var methodVars && elem var methodVarUsages
