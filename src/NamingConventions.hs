{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module NamingConventions where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic, simpleDiagnostic)
import Text.RE.TDFA.String

{- Package Name -}

checkPackageName :: CompilationUnit -> FilePath -> [Diagnostic]
checkPackageName (CompilationUnit (Just pDeckl) _ _) path = checkTLD (extractPackageNames pDeckl) path
checkPackageName (CompilationUnit {}) _ = []

checkTLD :: [String] -> FilePath -> [Diagnostic]
checkTLD [] _ = []
checkTLD (ident : idents) path
  | checkREOne ident = checkRestPN idents path
  | otherwise = simpleDiagnostic (packageNameMsg ident) path : checkRestPN idents path

checkRestPN :: [String] -> FilePath -> [Diagnostic]
checkRestPN idents path =
  idents
    & filter (not . checkRETwo)
    & map (\ident -> simpleDiagnostic (packageNameMsg ident) path)

packageNameMsg :: String -> String
packageNameMsg name = "PackageName element " ++ name ++ " does not match the specifications."

extractPackageNames :: PackageDecl -> [String]
extractPackageNames (PackageDecl (Name packageNames)) = map (\(Ident name) -> name) packageNames

{- MethodName -}

checkMethodName :: CompilationUnit -> FilePath -> [Diagnostic]
checkMethodName cUnit path = do
  (methodName, _) <- extractMethods cUnit
  checkMethodNames methodName path

checkMethodNames :: String -> FilePath -> [Diagnostic]
checkMethodNames name path
  | checkREThree name = mzero
  | otherwise = return (simpleDiagnostic (methodNameMsg name) path)
  where
    methodNameMsg name = "Methodname " ++ name ++ " does not match the specifications."

{- ParameterName -}

checkParameterName :: CompilationUnit -> FilePath -> [Diagnostic]
checkParameterName cUnit path = do
  methods <- extractFormalParams cUnit
  checkParameterNames methods path

extractFormalParams :: CompilationUnit -> [(String, [String])]
extractFormalParams cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ (Ident n) formalParams _ _ _) = return (n, extractParamName formalParams)
    extractBody _ = mzero
    extractParamName = map (\(FormalParam _ _ _ (VarId (Ident n))) -> n)

checkParameterNames :: (String, [String]) -> FilePath -> [Diagnostic]
checkParameterNames (methodName, formalParams) path =
  formalParams
    & filter (not . checkREThree)
    & map (\name -> methodDiagnostic methodName ("parameter " ++ name ++ " doesn't match the specifications.") path)

{- StaticVariableName -}

checkStaticVariableName :: CompilationUnit -> FilePath -> [Diagnostic]
checkStaticVariableName cUnit path = do
  membDecl <- extractStaticFieldNames cUnit
  checkStaticVariableNames membDecl path

extractStaticFieldNames :: CompilationUnit -> [String]
extractStaticFieldNames cUnit = do
  fieldNames <- universeBi cUnit
  extractMemberDecl fieldNames
  where
    extractMemberDecl (FieldDecl modifier _ varDecls)
      | Static `elem` modifier = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecls
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkStaticVariableNames :: String -> FilePath -> [Diagnostic]
checkStaticVariableNames name path
  | checkREThree name = mzero
  | otherwise = return (simpleDiagnostic ("Static variable " ++ name ++ " doesn't match the specifications.") path)

{- LocalFinalVariableName -}

checkLocalFinalVariableName :: CompilationUnit -> FilePath -> [Diagnostic]
checkLocalFinalVariableName cUnit path = do
  (methodName, methodBody) <- extractMethods cUnit
  variables <- extractLocalFinalVariableNames methodBody
  checkLocalFinalVariableNames methodName variables path

extractLocalFinalVariableNames :: MethodBody -> [String]
extractLocalFinalVariableNames cUnit = do
  fieldNames <- universeBi cUnit
  extractMemberDecl fieldNames
  where
    extractMemberDecl (LocalVars modifier _ varDecls)
      | Final `elem` modifier = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecls
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkLocalFinalVariableNames :: String -> String -> FilePath -> [Diagnostic]
checkLocalFinalVariableNames methodName name path
  | checkREThree name = mzero
  | otherwise = return (methodDiagnostic methodName ("Local final variable " ++ name ++ " doesn't match the specifications") path)

{- LocalVariableName -}

checkLocalVariableName :: CompilationUnit -> FilePath -> [Diagnostic]
checkLocalVariableName cUnit path = do
  (methodName, methodBody) <- extractMethods cUnit
  variables <- extractNonLocalFinalVariableNames methodBody
  checkLocalNonFinalVariableNames methodName variables path

extractNonLocalFinalVariableNames :: MethodBody -> [String]
extractNonLocalFinalVariableNames cUnit = do
  fieldNames <- universeBi cUnit
  extractMemberDecl fieldNames
  where
    extractMemberDecl (ForLocalVars modifier _ varDecl)
      | Final `notElem` modifier = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecl
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkLocalNonFinalVariableNames :: String -> String -> FilePath -> [Diagnostic]
checkLocalNonFinalVariableNames methodName name path
  | checkREThree name = mzero
  | otherwise = return (methodDiagnostic methodName ("Local non-final variable " ++ name ++ " in for-Loop doesn't match the specifications") path)

{- MemberName -}

checkMemberName :: CompilationUnit -> FilePath -> [Diagnostic]
checkMemberName cUnit path = do
  varNames <- extractNonStaticFieldNames cUnit
  checkMemberNames varNames path

extractNonStaticFieldNames :: CompilationUnit -> [String]
extractNonStaticFieldNames cUnit = do
  fieldNames <- universeBi cUnit
  extractMemberDecl fieldNames
  where
    extractMemberDecl (FieldDecl modifier _ varDecls)
      | Static `notElem` modifier = map (\(VarDecl (VarId (Ident n)) _) -> n) varDecls
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkMemberNames :: String -> FilePath -> [Diagnostic]
checkMemberNames name path
  | checkREThree name = mzero
  | otherwise = return (simpleDiagnostic ("Instance variable " ++ name ++ " doesn't match the specifications.") path)

{- TypeName -}

checkTypeName :: CompilationUnit -> FilePath -> [Diagnostic]
checkTypeName cUnit = checkTypeNames (extractCLassesAndInterfaces cUnit ++ extractEnums cUnit)

extractCLassesAndInterfaces :: CompilationUnit -> [String]
extractCLassesAndInterfaces cUnit = do
  classesAndInterfaces <- universeBi cUnit
  extractCLassAndInterface classesAndInterfaces
  where
    extractCLassAndInterface (ClassTypeDecl (ClassDecl _ (Ident n) _ _ _ _)) = return n
    extractCLassAndInterface (InterfaceTypeDecl (InterfaceDecl _ _ (Ident n) _ _ _)) = return n
    extractCLassAndInterface _ = mzero

extractEnums :: CompilationUnit -> [String]
extractEnums cUnit = do
  enums <- universeBi cUnit
  extractEnum enums
  where
    extractEnum (EnumDecl _ (Ident n) _ _) = return n
    extractEnum _ = mzero

checkTypeNames :: [String] -> FilePath -> [Diagnostic]
checkTypeNames names path =
  names
    & filter (not . checkREFour)
    & map (\name -> simpleDiagnostic ("Type name " ++ name ++ " doesn't match the specifications.") path)

{- Regular Expressions -}

checkREOne :: String -> Bool
checkREOne ident = matched (ident ?=~ [re|^[a-z]*$|])

checkRETwo :: String -> Bool
checkRETwo ident = matched (ident ?=~ [re|^[a-zA-Z_][a-zA-Z0-9_]*$|])

checkREThree :: String -> Bool
checkREThree x = matched $ x ?=~ [re|^[a-z][a-zA-Z0-9]*$|]

checkREFour :: String -> Bool
checkREFour x = matched $ x ?=~ [re|^[A-Z][a-zA-Z0-9]*$|]
