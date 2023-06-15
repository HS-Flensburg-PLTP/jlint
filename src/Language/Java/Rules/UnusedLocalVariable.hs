{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.UnusedLocalVariable where

import Control.Monad (MonadPlus (..))
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods, extractVarName)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

checkMethodVars :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkMethodVars cUnit path = do
  (methodName, methodBody) <- extractMethods cUnit
  checkIfMethodVarsAreUsed (extractMethodVariables methodBody) (extractMethodVariableUsages methodBody) methodName path

extractMethodVariables :: MethodBody Parsed -> [String]
extractMethodVariables methodBody = do
  names :: BlockStmt Parsed <- universeBi methodBody
  extractNames names
  where
    extractNames (LocalVars _ _ _ varDecls) = map (\(VarDecl _ varId _) -> extractVarName varId) varDecls
    extractNames _ = mzero

extractMethodVariableUsages :: MethodBody Parsed -> [String]
extractMethodVariableUsages methodBody = do
  usedVariables <- universeBi methodBody
  extractUsedVariables usedVariables
  where
    extractUsedVariables (Name _ varList) = map (\(Ident _ n) -> n) varList

checkIfMethodVarsAreUsed :: [String] -> [String] -> String -> FilePath -> [RDF.Diagnostic]
checkIfMethodVarsAreUsed declaredVars usedVars _ path =
  declaredVars
    & filter (`notElem` usedVars)
    & map (\var -> RDF.rangeDiagnostic "Language.Java.Rules.UnusedLocalVariable" ("Variable " ++ var ++ " is declared but never used.") dummySourceSpan path)

checkClassVars :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkClassVars cUnit path =
  concatMap
    ( \var ->
        if checkClassVarUsages (extractMethods cUnit) var
          then mzero
          else [RDF.rangeDiagnostic "Language.Java.Rules.UnusedLocalVariable" ("LocalVariable " ++ var ++ " is declared but never used.") dummySourceSpan path]
    )
    (extractClassVars cUnit)

checkClassVarUsages :: [(String, MethodBody Parsed)] -> String -> Bool
checkClassVarUsages methods var =
  methods
    & any
      ( \(_, body) ->
          checkClassVarUsageInMethod var (extractMethodVariables body) (extractMethodVariableUsages body)
      )

extractClassVars :: CompilationUnit Parsed -> [String]
extractClassVars cUnit = do
  variables :: MemberDecl Parsed <- universeBi cUnit
  extractVars variables
  where
    extractVars (FieldDecl _ _ _ varDecls) = map (\(VarDecl _ varId _) -> extractVarName varId) varDecls
    extractVars _ = mzero

checkClassVarUsageInMethod :: String -> [String] -> [String] -> Bool
checkClassVarUsageInMethod var methodVars methodVarUsages =
  notElem var methodVars && elem var methodVarUsages
