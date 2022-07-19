module CheckNonFinalMethodAttributes (check) where

import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Language.Java.Syntax (ClassBody (..), ClassDecl (..), CompilationUnit (..), Decl (..), FormalParam (..), Ident (Ident), MemberDecl (..), Modifier (Final), TypeDecl (..), VarDeclId (VarDeclArray, VarId))
import RDF (Diagnostic (..), Location (..), Severity (..))

check :: CompilationUnit -> FilePath -> [Diagnostic]
check (CompilationUnit _ _ classtype) = runReader (checkTypeDecls classtype)

checkTypeDecls :: [TypeDecl] -> Reader FilePath [Diagnostic]
checkTypeDecls = concatMapM checkTypeDecl

checkTypeDecl :: TypeDecl -> Reader FilePath [Diagnostic]
checkTypeDecl (ClassTypeDecl classDecl) = checkClassType classDecl
checkTypeDecl (InterfaceTypeDecl _) = return []

checkClassType :: ClassDecl -> Reader FilePath [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = checkDecls body
checkClassType (EnumDecl {}) = return []

checkDecls :: [Decl] -> Reader FilePath [Diagnostic]
checkDecls = concatMapM checkDecl

checkDecl :: Decl -> Reader FilePath [Diagnostic]
checkDecl (MemberDecl memberDecl) = checkMemberDecl memberDecl
checkDecl (InitDecl _ _) = return []

checkMemberDecl :: MemberDecl -> Reader FilePath [Diagnostic]
checkMemberDecl (MethodDecl _ _ _ (Ident ident) formalParam _ _ _) = checkMethodDecl ident formalParam
checkMemberDecl _ = return []

checkMethodDecl :: String -> [FormalParam] -> Reader FilePath [Diagnostic]
checkMethodDecl _ [] = return []
checkMethodDecl ident (x : xs) = do
  cfp <- checkFormalParam x ident
  cmd <- checkMethodDecl ident xs
  return (cfp ++ cmd)

checkFormalParam :: FormalParam -> String -> Reader FilePath [Diagnostic]
checkFormalParam (FormalParam [] _ _ varid) ident = do
  path <- ask
  return [constructDiagnostic (varId varid) ident path]
checkFormalParam (FormalParam modifier _ _ varid) ident = do
  path <- ask
  return [constructDiagnostic (varId varid) ident path | Final `notElem` modifier]

varId :: VarDeclId -> String
varId (VarDeclArray varDeclId) = varId varDeclId
varId (VarId (Ident n)) = n

constructDiagnostic :: String -> String -> FilePath -> Diagnostic
constructDiagnostic fparam ident path =
  Diagnostic
    { message = "Argument " ++ fparam ++ " in " ++ ident ++ " is not declared as 'final'.",
      location =
        Location
          { path = path,
            range = Nothing
          },
      severity = WARNING,
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }
