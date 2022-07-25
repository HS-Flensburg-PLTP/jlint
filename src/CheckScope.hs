module CheckScope where

import AST (extractMethods, extractVarName)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

extractMethodVariables :: MethodBody -> [String]
extractMethodVariables methodBody = do
  names <- universeBi methodBody
  extractNames names
  where
    extractNames (LocalVars _ _ varDecls) = map (\(VarDecl varId _) -> extractVarName varId) varDecls
    extractNames _ = mzero

extractMethodStatements :: MethodBody -> [Stmt]
extractMethodStatements methodBody = do
  statements <- universeBi methodBody
  extractStatements statements
  where
    extractStatements (IfThen _ stmt) = [stmt]
    extractStatements (IfThenElse _ stmt1 stmt2) = [stmt1, stmt2]
    extractStatements (While _ stmt) = [stmt]
    extractStatements (BasicFor _ _ _ stmt) = [stmt]
    extractStatements (EnhancedFor _ _ _ _ stmt) = [stmt]
    extractStatements (Do stmt _) = [stmt]
    extractStatements _ = mzero

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  (methodName, methodBody) <- extractMethods cUnit
  variables <- extractMethodVariables methodBody
  if countVarCalls (extractMethodStatements methodBody) variables == 1
    then [methodDiagnostic methodName ("The scope of variable " ++ variables ++ " can be reduced.") path]
    else mzero

countVarCalls :: [Stmt] -> String -> Int
countVarCalls [] _ = 0
countVarCalls (x : xs) var
  | or (checkIfElement x var) = 1 + countVarCalls xs var
  | otherwise = 0 + countVarCalls xs var

checkIfElement :: Stmt -> String -> [Bool]
checkIfElement stmt var = do
  elements <- universeBi stmt
  checkStatement elements var
  where
    checkStatement (Name varList) var = [Ident var `elem` varList]
