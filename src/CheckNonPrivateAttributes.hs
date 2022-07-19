module CheckNonPrivateAttributes where

import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
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
    VarDeclId (VarDeclArray, VarId),
  )
import RDF (Diagnostic (..), Location (..), Severity (..))

check :: CompilationUnit -> FilePath -> [Diagnostic]
check (CompilationUnit _ _ classtype) = runReader (checkTypeDecls classtype)

checkTypeDecls :: [TypeDecl] -> Reader FilePath [Diagnostic]
checkTypeDecls = concatMapM checkTypeDecl

checkTypeDecl :: TypeDecl -> Reader FilePath [Diagnostic]
checkTypeDecl (ClassTypeDecl cd) = checkClassType cd
checkTypeDecl (InterfaceTypeDecl _) = return []

checkClassType :: ClassDecl -> Reader FilePath [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = checkDecls body
checkClassType (EnumDecl {}) = return []

checkDecls :: [Decl] -> Reader FilePath [Diagnostic]
checkDecls = concatMapM checkDecl

checkDecl :: Decl -> Reader FilePath [Diagnostic]
checkDecl (MemberDecl md) = checkMemberDecl md
checkDecl (InitDecl _ _) = return []

checkMemberDecl :: MemberDecl -> Reader FilePath [Diagnostic]
checkMemberDecl (FieldDecl _ _ []) = return []
checkMemberDecl (FieldDecl mods _ vardecl) = varDecls (map (\(VarDecl vardeclId _) -> varId vardeclId) vardecl) mods
checkMemberDecl (MethodDecl {}) = return []
checkMemberDecl (ConstructorDecl {}) = return []
checkMemberDecl (MemberClassDecl {}) = return []
checkMemberDecl (MemberInterfaceDecl {}) = return []

varId :: VarDeclId -> String
varId (VarDeclArray varDeclId) = varId varDeclId
varId (VarId (Ident n)) = n

varDecls :: [String] -> [Modifier] -> Reader FilePath [Diagnostic]
varDecls strings mods = concatMapM (checkFieldDecl mods) strings

checkFieldDecl :: [Modifier] -> String -> Reader FilePath [Diagnostic]
checkFieldDecl [] varname = do
  fpath <- ask
  return [constructDiagnostic varname fpath]
checkFieldDecl modifier varname = do
  fpath <- ask
  return [constructDiagnostic varname fpath | Private `notElem` modifier]

constructDiagnostic :: String -> FilePath -> Diagnostic
constructDiagnostic varname fpath =
  Diagnostic
    { message = "Attribute " ++ varname ++ " is not declared as 'private'.",
      location =
        Location
          { path = fpath,
            locationRange = Nothing
          },
      severity = WARNING,
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }
