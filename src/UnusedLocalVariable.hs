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
  checkIfUsed (extractMethodVariables methodBody) (extractMethodVariableUsages methodBody) methodName path

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

checkIfUsed :: [String] -> [String] -> String -> FilePath -> [Diagnostic]
checkIfUsed declaredVars usedVars methodName path =
  declaredVars
    & filter (`notElem` usedVars)
    & map (\var -> methodDiagnostic methodName ("Variable " ++ var ++ " is declared but never used.") path)

checkAllVars :: CompilationUnit -> FilePath -> [Diagnostic]
checkAllVars cUnit path =
  concatMap (\var -> if test (extractMethods cUnit) var then mzero else [simpleDiagnostic ("LocalVariable " ++ var ++ " is declared but never used.") path]) (extractClassVars cUnit)

test :: [(String, MethodBody)] -> String -> Bool
test methods var =
  methods
    & any
      ( \(_, body) ->
          checkelDiCheck var (extractMethodVariables body) (extractMethodVariableUsages body)
      )

extractClassVars :: CompilationUnit -> [String]
extractClassVars cUnit = do
  vars <- universeBi cUnit
  extractVars vars
  where
    extractVars (FieldDecl _ _ varDecls) = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecls
    extractVars _ = mzero

checkelDiCheck :: String -> [String] -> [String] -> Bool
checkelDiCheck var methodVars methodVarUsages =
  notElem var methodVars && elem var methodVarUsages
