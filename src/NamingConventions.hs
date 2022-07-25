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
  | matched (ident ?=~ reTopLevelDomain) = checkRestPN idents path
  | otherwise = simpleDiagnostic (packageNameMsg ident) path : checkRestPN idents path

checkRestPN :: [String] -> FilePath -> [Diagnostic]
checkRestPN idents path =
  idents
    & filter
      ( \ident ->
          not
            ( matched (ident ?=~ reRestDomain)
            )
      )
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
  | matched (name ?=~ reCamelCase) = mzero
  | otherwise = return (simpleDiagnostic (methodNameMsg name) path)
  where
    methodNameMsg name = "Methodname " ++ name ++ " does not match the specifications."

{- ParameterName -}

checkParameterName :: CompilationUnit -> FilePath -> [Diagnostic]
checkParameterName cUnit path = do
  method <- extractFormalParams cUnit
  checkParameterNames method path

extractFormalParams :: CompilationUnit -> [(String, [String])]
extractFormalParams cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ (Ident n) formalParams _ _ _) = return (n, map (\(FormalParam _ _ _ varDeclIds) -> extractFormalParamName varDeclIds) formalParams)
    extractBody _ = mzero

checkParameterNames :: (String, [String]) -> FilePath -> [Diagnostic]
checkParameterNames (methodName, formalParams) path =
  formalParams
    & filter (\name -> not (matched (name ?=~ reCamelCase)))
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
      | Static `elem` modifier = map extractVarName varDecls
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkStaticVariableNames :: String -> FilePath -> [Diagnostic]
checkStaticVariableNames name path
  | matched (name ?=~ reCamelCase) = mzero
  | otherwise = return (simpleDiagnostic ("Static variable " ++ name ++ " doesn't match the specifications.") path)

{- LocalFinalVariableName & LocalVariableName -}

checkLocalName :: CompilationUnit -> FilePath -> [Diagnostic]
checkLocalName cUnit path = do
  method <- extractMethods cUnit
  extractLocalFinalVariableNames2 method path

extractLocalFinalVariableNames2 :: (String, MethodBody) -> FilePath -> [Diagnostic]
extractLocalFinalVariableNames2 (methodName, methodBody) path = do
  fieldNames <- universeBi methodBody
  extractMemberDecl fieldNames
  where
    extractMemberDecl (LocalVars modifier _ varDecls)
      | Final `elem` modifier =
          map extractVarName varDecls
            & filter (\name -> not (matched (name ?=~ reCamelCase)))
            & map (\name -> methodDiagnostic methodName ("Local final variable " ++ name ++ " doesn't match the specifications") path)
      | otherwise =
          map extractVarName varDecls
            & filter (\name -> not (matched (name ?=~ reCamelCase)))
            & map (\name -> methodDiagnostic methodName ("Local non-final variable " ++ name ++ " doesn't match the specifications") path)
    extractMemberDecl _ = mzero

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
      | Static `notElem` modifier = map extractVarName varDecls
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkMemberNames :: String -> FilePath -> [Diagnostic]
checkMemberNames name path
  | matched (name ?=~ reCamelCase) = mzero
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
    & filter (\name -> not (matched (name ?=~ rePascalCase)))
    & map (\name -> simpleDiagnostic ("Type name " ++ name ++ " doesn't match the specifications.") path)

{- Regular Expressions -}

reTopLevelDomain :: RE
reTopLevelDomain = [re|^[a-z]*$|]

reRestDomain :: RE
reRestDomain = [re|^[a-zA-Z_][a-zA-Z0-9_]*$|]

reCamelCase :: RE
reCamelCase = [re|^[a-z][a-zA-Z0-9]*$|]

rePascalCase :: RE
rePascalCase = [re|^[A-Z][a-zA-Z0-9]*$|]

{- Helper -}

extractVarName :: VarDecl -> String
extractVarName (VarDecl id _) = extractFormalParamName id

extractFormalParamName :: VarDeclId -> String
extractFormalParamName (VarId (Ident name)) = name
extractFormalParamName (VarDeclArray varDeclId) = extractFormalParamName varDeclId
