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

check :: CompilationUnit -> [Diagnostic]
check (CompilationUnit _ _ classtype) = checkTypeDecls classtype

checkTypeDecls :: [TypeDecl] -> [Diagnostic]
checkTypeDecls = concatMap checkTypeDecl

checkTypeDecl :: TypeDecl -> [Diagnostic]
checkTypeDecl (ClassTypeDecl cd) = checkClassType cd
checkTypeDecl (InterfaceTypeDecl _) = []

checkClassType :: ClassDecl -> [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = checkDecls body
checkClassType (EnumDecl {}) = []

checkDecls :: [Decl] -> [Diagnostic]
checkDecls = concatMap checkDecl

checkDecl :: Decl -> [Diagnostic]
checkDecl (MemberDecl md) = checkMemberDecl md
checkDecl (InitDecl _ _) = []

checkMemberDecl :: MemberDecl -> [Diagnostic]
checkMemberDecl (FieldDecl mods _ ((VarDecl (VarId (Ident n)) _) : _)) = checkFieldDecl mods n
checkMemberDecl (MethodDecl {}) = []
checkMemberDecl (ConstructorDecl {}) = []
checkMemberDecl (MemberClassDecl {}) = []
checkMemberDecl (MemberInterfaceDecl {}) = []

checkFieldDecl :: [Modifier] -> String -> [Diagnostic]
checkFieldDecl [] varname =
  [ constructDiagnostic varname
  ]
checkFieldDecl modifier varname =
  [ constructDiagnostic varname
    | Private `notElem` modifier
  ]

constructDiagnostic :: String -> Diagnostic
constructDiagnostic varname =
  Diagnostic
    { message = "Attribute " ++ varname ++ " is not deklared as 'final'",
      location =
        Location
          { path = "test/Strings2.java",
            locationRange = Nothing
          },
      severity = Just (Left "WARNING"),
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }
