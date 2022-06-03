module CheckNonPrivateAttributes where

import Language.Java.Syntax
  ( ClassBody (ClassBody),
    ClassDecl (..),
    CompilationUnit (..),
    Decl (..),
    Ident (Ident),
    MemberDecl (..),
    Modifier (Private),
    TypeDecl (..),
    VarDecl (VarDecl),
    VarDeclId (VarId),
  )
import RDF (Diagnostic (..), Location (..))

data Error = ClassVarNotPrivate {var :: String}
  deriving (Show)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check (CompilationUnit _ _ classtype) = checkTypeDecls classtype

checkTypeDecls :: [TypeDecl] -> FilePath -> [Diagnostic]
checkTypeDecls [] _ = []
checkTypeDecls (x:xs) path = checkTypeDecl x path ++ checkTypeDecls xs path

checkTypeDecl :: TypeDecl -> FilePath -> [Diagnostic]
checkTypeDecl (ClassTypeDecl cd) path = checkClassType cd path
checkTypeDecl (InterfaceTypeDecl _) _= []

checkClassType :: ClassDecl -> FilePath -> [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) path = checkDecls body path
checkClassType (EnumDecl {}) _ = []

checkDecls :: [Decl] -> FilePath -> [Diagnostic]
checkDecls [] _ = []
checkDecls (x:xs) path = checkDecl x path ++ checkDecls xs path

checkDecl :: Decl -> FilePath -> [Diagnostic]
checkDecl (MemberDecl md) path = checkMemberDecl md path
checkDecl (InitDecl _ _) _= []

checkMemberDecl :: MemberDecl -> FilePath -> [Diagnostic]
checkMemberDecl (FieldDecl mods _ ((VarDecl (VarId (Ident n)) _) : _)) path = checkFieldDecl mods n path
checkMemberDecl (MethodDecl {}) _= []
checkMemberDecl (ConstructorDecl {}) _= []
checkMemberDecl (MemberClassDecl {}) _= []
checkMemberDecl (MemberInterfaceDecl {}) _ = []

checkFieldDecl :: [Modifier] -> String -> FilePath -> [Diagnostic]
checkFieldDecl [] varname path =
  [ constructDiagnostic varname path
  ]
checkFieldDecl modifier varname  path =
  [ constructDiagnostic varname path
    | Private `notElem` modifier
  ]

constructDiagnostic :: String -> FilePath ->Diagnostic
constructDiagnostic varname path =
  Diagnostic
    { message = "Attribute " ++ varname ++ " is not declared as 'private'.",
      location =
        Location
          { path = path,
            locationRange = Nothing
          },
      severity = Just (Left "WARNING"),
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }
