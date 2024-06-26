module Language.Java.Rules.UnusedLocalVariable where

import Control.Monad (mzero)
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List.NonEmpty as NonEmpty
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
  names <- universeBi methodBody :: [BlockStmt Parsed]
  extractNames names
  where
    extractNames (LocalVars _ _ _ varDecls) = map (\(VarDecl _ varId _) -> extractVarName varId) (NonEmpty.toList varDecls)
    extractNames _ = mzero

extractMethodVariableUsages :: MethodBody Parsed -> [String]
extractMethodVariableUsages methodBody = do
  usedVariables <- universeBi methodBody
  extractUsedVariables usedVariables
  where
    extractUsedVariables (Name _ varList) = map (\(Ident _ n) -> n) (NonEmpty.toList varList)

checkIfMethodVarsAreUsed :: [String] -> [String] -> String -> FilePath -> [RDF.Diagnostic]
checkIfMethodVarsAreUsed declaredVars usedVars _ path =
  declaredVars
    & filter (`notElem` usedVars)
    & map
      ( \var ->
          RDF.rangeDiagnostic
            "Language.Java.Rules.UnusedLocalVariable"
            ["Die Variable", var, "wird deklariert, aber nie verwendet."]
            dummySourceSpan
            path
      )

checkClassVars :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkClassVars cUnit path =
  concatMap
    ( \var ->
        if checkClassVarUsages (extractMethods cUnit) var
          then mzero
          else
            [ RDF.rangeDiagnostic
                "Language.Java.Rules.UnusedLocalVariable"
                ["Die lokale Variable", var, "wird deklariert, aber nie verwendet."]
                dummySourceSpan
                path
            ]
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
  variables <- universeBi cUnit
  extractVars variables
  where
    extractVars :: MemberDecl Parsed -> [String]
    extractVars (FieldDecl _ _ _ varDecls) = map (\(VarDecl _ varId _) -> extractVarName varId) (NonEmpty.toList varDecls)
    extractVars _ = mzero

checkClassVarUsageInMethod :: String -> [String] -> [String] -> Bool
checkClassVarUsageInMethod var methodVars methodVarUsages =
  notElem var methodVars && elem var methodVarUsages
