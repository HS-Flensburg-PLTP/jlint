module UnusedLocalVariable where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
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
    extractUsedVariables _ = mzero

checkIfUsed :: [String] -> [String] -> String -> FilePath -> [Diagnostic]
checkIfUsed declaredVars usedVars methodName path =
  declaredVars
    & filter (`notElem` usedVars)
    & map (\var -> methodDiagnostic methodName ("Variable " ++ var ++ " is declared but never used.") path)
