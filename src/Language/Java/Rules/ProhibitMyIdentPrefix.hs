{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitMyIdentPrefix where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Parser (formalParam, formalParams, lambdaExp)
import Language.Java.Syntax
import qualified RDF
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check (CompilationUnit pkgDecl _ typeDecls) path =
  checkPackageDecl pkgDecl path
    ++ ( do
           interfaceDecl <- universeBi typeDecls
           checkInterfaceDecl interfaceDecl path
       )
    ++ ( do
           classDecl <- universeBi typeDecls
           checkClassDecl classDecl path
       )
    ++ ( do
           memberDecl <- universeBi typeDecls
           checkMemberDecl memberDecl path
       )
    ++ ( do
           localVar <- universeBi typeDecls
           checkLocalVar localVar path
       )
    ++ ( do
           annotation <- universeBi typeDecls
           checkAnnotation annotation path
       )
    ++ ( do
           lambdaExp <- universeBi typeDecls
           checkLambda lambdaExp path
       )

checkIdent :: Ident -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkIdent (Ident ident) sourceSpan path
  | matched (ident ?=~ [re|^([Mm]y)|MY|]) =
    return
      ( RDF.rangeDiagnostic
          "Language.Java.Rules.ProhibitMyIdentPrefix"
          ("Nicht erlaubtes 'My'-Prefix gefunden: " ++ ident)
          sourceSpan
          path
      )
  | otherwise = mzero

checkPackageDecl :: Maybe PackageDecl -> FilePath -> [RDF.Diagnostic]
checkPackageDecl Nothing _ = mzero
checkPackageDecl (Just (PackageDecl name)) path = checkName name dummySourceSpan path

checkName :: Name -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkName name sourcespan path = do
  ident <- universeBi name
  checkIdent ident sourcespan path

checkTypeDecl :: TypeDecl -> FilePath -> [RDF.Diagnostic]
checkTypeDecl typeDecl path = case typeDecl of
  ClassTypeDecl classDecl -> checkClassDecl classDecl path
  InterfaceTypeDecl interfaceDecl -> checkInterfaceDecl interfaceDecl path

checkClassDecl :: ClassDecl -> FilePath -> [RDF.Diagnostic]
checkClassDecl (ClassDecl sourceSpan _ ident _ _ _ _) path = checkIdent ident sourceSpan path
checkClassDecl (EnumDecl sourceSpan _ ident _ enumBody) path =
  checkIdent ident sourceSpan path
    ++ ( do
           ident <- universeBi enumBody
           checkIdent ident sourceSpan path
       )
checkClassDecl (RecordDecl sourceSpan _ ident _ _ _ _) path = checkIdent ident sourceSpan path

checkInterfaceDecl :: InterfaceDecl -> FilePath -> [RDF.Diagnostic]
checkInterfaceDecl (InterfaceDecl sourceSpan _ _ ident _ _ _ _) = checkIdent ident sourceSpan

checkMemberDecl :: MemberDecl -> FilePath -> [RDF.Diagnostic]
checkMemberDecl (FieldDecl sourceSpan _ _ varDecls) path = do
  ident <- universeBi varDecls
  checkIdent ident sourceSpan path
checkMemberDecl (MethodDecl sourceSpan _ _ _ ident _ _ _ _) path = checkIdent ident sourceSpan path
checkMemberDecl (ConstructorDecl sourceSpan _ _ ident _ _ _) path = checkIdent ident sourceSpan path
checkMemberDecl _ _ = mzero

checkLocalVar :: BlockStmt -> FilePath -> [RDF.Diagnostic]
checkLocalVar (LocalVars sourceSpan _ _ varDecls) path = do
  ident <- universeBi varDecls
  checkIdent ident sourceSpan path
checkLocalVar _ _ = mzero

checkAnnotation :: Annotation -> FilePath -> [RDF.Diagnostic]
checkAnnotation (NormalAnnotation sourcespan name _) = checkName name sourcespan
checkAnnotation (SingleElementAnnotation sourcespan name _) = checkName name sourcespan
checkAnnotation (MarkerAnnotation sourcespan name) = checkName name sourcespan

checkLambda :: LambdaParams -> FilePath -> [RDF.Diagnostic]
checkLambda (LambdaSingleParam ident) path = checkIdent ident dummySourceSpan path
checkLambda (LambdaFormalParams formalParams) path = do
  ident <- universeBi formalParams
  checkIdent ident dummySourceSpan path
checkLambda (LambdaInferredParams idents) path = concatMap (\ident -> checkIdent ident dummySourceSpan path) idents

{-
checkPackageDeclName
checkClassDeclIdent
checkEnumDeclIdent
checkInterfaceDeclIdent
checkMemberDecl
-}
{-
VarDecl Enum
Methoden
Class Interface usw
Package

-}

{-
Packagedecl name
Typedecl
    classdecl ident
    enumdecl
    interface int
decl
    memberdecl

extractIdent dingdasidenthat =

-}
