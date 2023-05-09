{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.NamingConventions (check) where

import Control.Monad (MonadPlus (..))
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Extra (none)
import Language.Java.AST (extractMethods)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  concatMap
    (\c -> c cUnit path)
    [ checkPackageName,
      checkMethodName,
      checkParameterName,
      checkStaticVariableName,
      checkLocalName,
      checkMemberName,
      checkTypeName
    ]

{- Package Name -}

checkPackageName :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkPackageName (CompilationUnit (Just pDeckl) _ _) path = checkTLD (extractPackageNames pDeckl) path
checkPackageName (CompilationUnit {}) _ = []

checkTLD :: [String] -> FilePath -> [RDF.Diagnostic]
checkTLD [] _ = []
checkTLD (ident : idents) path
  | matched (ident ?=~ reTopLevelDomain) = checkRestPN idents path
  | otherwise = RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" (packageNameMsg ident) dummySourceSpan path : checkRestPN idents path

checkRestPN :: [String] -> FilePath -> [RDF.Diagnostic]
checkRestPN idents path =
  idents
    & filter
      ( \ident ->
          not
            ( matched (ident ?=~ reRestDomain)
            )
      )
    & map (\ident -> RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" (packageNameMsg ident) dummySourceSpan path)

packageNameMsg :: String -> String
packageNameMsg name = "PackageName element " ++ name ++ " does not match the specifications."

extractPackageNames :: PackageDecl -> [String]
extractPackageNames (PackageDecl (Name _ packageNames)) = map (\(Ident _ name) -> name) packageNames

{- MethodName -}

checkMethodName :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkMethodName cUnit path = do
  (methodName, _) <- extractMethods cUnit
  checkMethodNames methodName path

checkMethodNames :: String -> FilePath -> [RDF.Diagnostic]
checkMethodNames name path
  | matched (name ?=~ reCamelCase) = mzero
  | otherwise = return (RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" (methodNameMsg name) dummySourceSpan path)
  where
    methodNameMsg name = "Methodname " ++ name ++ " does not match the specifications."

{- ParameterName -}

checkParameterName :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkParameterName cUnit path = do
  method <- extractFormalParams cUnit
  checkParameterNames method path

extractFormalParams :: CompilationUnit -> [(String, [String])]
extractFormalParams cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ _ (Ident _ n) formalParams _ _ _) = return (n, map (\(FormalParam _ _ _ _ varDeclIds) -> extractFormalParamName varDeclIds) formalParams)
    extractBody _ = mzero

checkParameterNames :: (String, [String]) -> FilePath -> [RDF.Diagnostic]
checkParameterNames (_, formalParams) path =
  formalParams
    & filter (\name -> not (matched (name ?=~ reCamelCase)))
    & map (\name -> RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" ("parameter " ++ name ++ " doesn't match the specifications.") dummySourceSpan path)

{- StaticVariableName -}

checkStaticVariableName :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkStaticVariableName cUnit path = do
  membDecl <- extractStaticFieldNames cUnit
  checkStaticVariableNames membDecl path

extractStaticFieldNames :: CompilationUnit -> [String]
extractStaticFieldNames cUnit = do
  fieldNames <- universeBi cUnit
  extractMemberDecl fieldNames
  where
    extractMemberDecl (FieldDecl _ modifier _ varDecls)
      | any (eq IgnoreSourceSpan Static) modifier = map extractVarName varDecls
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkStaticVariableNames :: String -> FilePath -> [RDF.Diagnostic]
checkStaticVariableNames name path
  | matched (name ?=~ reCamelCase) = mzero
  | otherwise = return (RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" ("Static variable " ++ name ++ " doesn't match the specifications.") dummySourceSpan path)

{- LocalFinalVariableName & LocalVariableName -}

checkLocalName :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkLocalName cUnit path = do
  method <- extractMethods cUnit
  extractLocalFinalVariableNames2 method path

extractLocalFinalVariableNames2 :: (String, MethodBody) -> FilePath -> [RDF.Diagnostic]
extractLocalFinalVariableNames2 (_, methodBody) path = do
  fieldNames <- universeBi methodBody
  extractMemberDecl fieldNames
  where
    extractMemberDecl (LocalVars _ modifier _ varDecls)
      | any (eq IgnoreSourceSpan Final) modifier =
          map extractVarName varDecls
            & filter (\name -> not (matched (name ?=~ reCamelCase)))
            & map (\name -> RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" ("Local final variable " ++ name ++ " doesn't match the specifications") dummySourceSpan path)
      | otherwise =
          map extractVarName varDecls
            & filter (\name -> not (matched (name ?=~ reCamelCase)))
            & map (\name -> RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" ("Local non-final variable " ++ name ++ " doesn't match the specifications") dummySourceSpan path)
    extractMemberDecl _ = mzero

{- MemberName -}

checkMemberName :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkMemberName cUnit path = do
  varNames <- extractNonStaticFieldNames cUnit
  checkMemberNames varNames path

extractNonStaticFieldNames :: CompilationUnit -> [String]
extractNonStaticFieldNames cUnit = do
  fieldNames <- universeBi cUnit
  extractMemberDecl fieldNames
  where
    extractMemberDecl (FieldDecl _ modifier _ varDecls)
      | none (eq IgnoreSourceSpan Static) modifier = map extractVarName varDecls
      | otherwise = mzero
    extractMemberDecl _ = mzero

checkMemberNames :: String -> FilePath -> [RDF.Diagnostic]
checkMemberNames name path
  | matched (name ?=~ reCamelCase) = mzero
  | otherwise = return (RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" ("Instance variable " ++ name ++ " doesn't match the specifications.") dummySourceSpan path)

{- TypeName -}

checkTypeName :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkTypeName cUnit = checkTypeNames (extractCLassesAndInterfaces cUnit ++ extractEnums cUnit)

extractCLassesAndInterfaces :: CompilationUnit -> [String]
extractCLassesAndInterfaces cUnit = do
  classesAndInterfaces <- universeBi cUnit
  extractCLassAndInterface classesAndInterfaces
  where
    extractCLassAndInterface (ClassTypeDecl _ (ClassDecl _ _ (Ident _ n) _ _ _ _)) = return n
    extractCLassAndInterface (InterfaceTypeDecl _ (InterfaceDecl _ _ _ (Ident _ n) _ _ _ _)) = return n
    extractCLassAndInterface _ = mzero

extractEnums :: CompilationUnit -> [String]
extractEnums cUnit = do
  enums <- universeBi cUnit
  extractEnum enums
  where
    extractEnum (EnumDecl _ _ (Ident _ n) _ _) = return n
    extractEnum _ = mzero

checkTypeNames :: [String] -> FilePath -> [RDF.Diagnostic]
checkTypeNames names path =
  names
    & filter (\name -> not (matched (name ?=~ rePascalCase)))
    & map (\name -> RDF.rangeDiagnostic "Language.Java.Rules.NamingConventions" ("Type name " ++ name ++ " doesn't match the specifications.") dummySourceSpan path)

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
extractVarName (VarDecl _ id _) = extractFormalParamName id

extractFormalParamName :: VarDeclId -> String
extractFormalParamName (VarId (Ident _ name)) = name
extractFormalParamName (VarDeclArray _ varDeclId) = extractFormalParamName varDeclId
