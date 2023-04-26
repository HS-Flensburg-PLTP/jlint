{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitMyIdentPrefix where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
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

{-
  (class, package, method, blaaaa) <- universeBi cUnit
  checkIdent ident path
-}
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
checkPackageDecl (Just (PackageDecl name)) path = do
  ident <- universeBi name
  checkIdent ident dummySourceSpan path

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
