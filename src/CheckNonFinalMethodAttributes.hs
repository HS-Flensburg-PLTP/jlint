module CheckNonFinalMethodAttributes (check) where

import Language.Java.Syntax (ClassBody (..), ClassDecl (..), CompilationUnit (..), Decl (..), FormalParam (..), Ident (Ident), MemberDecl (..), Modifier (Final), TypeDecl (..), VarDeclId (VarId))
import RDF (Diagnostic (..), Location (..))
import Control.Monad.Reader

data Error = FuncVarNotFinal {func :: String, var :: String}
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
checkTypeDecl (ClassTypeDecl classDecl) = checkClassType classDecl
checkTypeDecl (InterfaceTypeDecl _) = return []

checkClassType :: ClassDecl -> Reader FilePath [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = checkDecls body
checkClassType (EnumDecl _ _ _ _) = return []

checkDecls :: [Decl] -> Reader FilePath [Diagnostic]
checkDecls [] = return []
checkDecls (x:xs) =
  do
    dcl <- checkDecl x
    cdcls <- checkDecls xs
    return (dcl ++ cdcls)

checkDecl :: Decl -> Reader FilePath [Diagnostic]
checkDecl (MemberDecl memberDecl) = checkMemberDecl memberDecl
checkDecl (InitDecl _ _) = return []

checkMemberDecl :: MemberDecl -> Reader FilePath [Diagnostic]
checkMemberDecl (MethodDecl _ _ _ (Ident ident) formalParam _ _ _) = checkMethodDecl ident formalParam
checkMemberDecl _ = return []

checkMethodDecl :: String -> [FormalParam] -> Reader FilePath [Diagnostic]
checkMethodDecl _ [] = return []
checkMethodDecl ident (x:xs) =
  do 
    cfp <- checkFormalParam x ident
    cmd <- checkMethodDecl ident xs
    return (cfp ++ cmd)



checkFormalParam :: FormalParam -> String -> Reader FilePath [Diagnostic]
checkFormalParam (FormalParam modifier _ _ (VarId (Ident n))) ident =
      do
        path <- ask
        return [constructDiagnostic n ident path | Final `notElem` modifier ]

constructDiagnostic :: String -> String -> FilePath -> Diagnostic
constructDiagnostic fparam ident path =
  Diagnostic
      { message = "Argument " ++ fparam ++ " in " ++ ident ++ " is not declared as 'final'.",
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