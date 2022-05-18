module CheckNonPrivateAttributes where

import Language.Java.Syntax
    ( VarDeclId(VarId),
      VarDecl(VarDecl),
      MemberDecl(..),
      Decl(..),
      ClassBody(ClassBody),
      Ident(Ident),
      Modifier(Private),
      ClassDecl(..),
      TypeDecl(..),
      CompilationUnit(..) )

data Error = ClassVarNotPrivate {var :: String}
  deriving (Show)

check :: CompilationUnit -> [Error]
check (CompilationUnit _ _ classtype) =
      checkTypeDecls classtype

checkTypeDecls :: [TypeDecl] -> [Error]
checkTypeDecls [] = []
checkTypeDecls typedecls = concatMap checkTypeDecl typedecls

checkTypeDecl :: TypeDecl -> [Error]
checkTypeDecl (ClassTypeDecl cd) = checkClassType cd
checkTypeDecl (InterfaceTypeDecl _) = []

checkClassType :: ClassDecl -> [Error]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = checkDecls body
checkClassType (EnumDecl {}) = []

checkDecls :: [Decl] -> [Error]
checkDecls [] = []
checkDecls decls = concatMap checkDecl decls

checkDecl :: Decl -> [Error]
checkDecl (MemberDecl md) = checkMemberDecl md
checkDecl (InitDecl _ _) = []

checkMemberDecl :: MemberDecl -> [Error]
checkMemberDecl (FieldDecl mods _ ((VarDecl (VarId (Ident n)) _) : _) ) = checkFieldDecl mods n
checkMemberDecl (MethodDecl {}) = []
checkMemberDecl (ConstructorDecl {}) = []
checkMemberDecl (MemberClassDecl {}) = []
checkMemberDecl (MemberInterfaceDecl {}) = []


checkFieldDecl :: [Modifier] -> String -> [Error]
checkFieldDecl [] varname = [ClassVarNotPrivate {var = varname}]
checkFieldDecl modifier varname = [ClassVarNotPrivate {var = varname} | Private `notElem` modifier]