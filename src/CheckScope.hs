module CheckScope where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = concatMap (\var -> if countMethodsWithCalls (extractMethods cUnit) var == 1 then [simpleDiagnostic ("Scope of variable " ++ var ++ " can be reduced.") path] else mzero) (extractClassVars cUnit)

countMethodsWithCalls :: [(String, MethodBody)] -> String -> Int
countMethodsWithCalls methods var =
  methods
    & filter (\(_, body) -> checkClassVarUsageInMethod var (extractMethodVariables body) (extractMethodVariableUsages body))
    & length

checkClassVarUsageInMethod :: String -> [String] -> [String] -> Bool
checkClassVarUsageInMethod var methodVars methodVarUsages =
  notElem var methodVars && elem var methodVarUsages

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

extractClassVars :: CompilationUnit -> [String]
extractClassVars cUnit = do
  variables <- universeBi cUnit
  extractVars variables
  where
    extractVars (FieldDecl _ _ varDecls) = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecls
    extractVars _ = mzero
