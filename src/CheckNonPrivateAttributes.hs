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
import Control.Monad.Reader

data Error = ClassVarNotPrivate {var :: String}
  deriving (Show)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check (CompilationUnit _ _ classtype) path = runReader (checkTypeDecls classtype) path

checkTypeDecls :: [TypeDecl] -> Reader FilePath [Diagnostic]
checkTypeDecls [] = return []
checkTypeDecls (x:xs) = 
    do
      td <- checkTypeDecl x
      ctds <- checkTypeDecls xs
      return (td ++ ctds)


checkTypeDecl :: TypeDecl -> Reader FilePath [Diagnostic]
checkTypeDecl (ClassTypeDecl cd) = checkClassType cd
checkTypeDecl (InterfaceTypeDecl _) = return []

checkClassType :: ClassDecl -> Reader FilePath [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = checkDecls body
checkClassType (EnumDecl {}) = return []

checkDecls :: [Decl] -> Reader FilePath [Diagnostic]
checkDecls [] = return []
checkDecls (x:xs) =
  do
    dcl <- checkDecl x
    cdcls <- checkDecls xs
    return (dcl ++ cdcls)


checkDecl :: Decl -> Reader FilePath [Diagnostic]
checkDecl (MemberDecl md) = checkMemberDecl md
checkDecl (InitDecl _ _) = return []

checkMemberDecl :: MemberDecl -> Reader FilePath [Diagnostic]
checkMemberDecl (FieldDecl mods _ ((VarDecl (VarId (Ident n)) _) : _)) = checkFieldDecl mods n
checkMemberDecl (MethodDecl {}) = return []
checkMemberDecl (ConstructorDecl {}) = return []
checkMemberDecl (MemberClassDecl {}) = return []
checkMemberDecl (MemberInterfaceDecl {}) = return []

checkFieldDecl :: [Modifier] -> String -> Reader FilePath [Diagnostic]
checkFieldDecl [] varname =
  do
    path <- ask
    return [ constructDiagnostic varname path ]
checkFieldDecl modifier varname =
  do
    path <- ask
    return [ constructDiagnostic varname path | Private `notElem` modifier]

constructDiagnostic :: String -> FilePath -> Diagnostic
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
