module CheckNonFinalMethodAttributes (check) where

import Data.Maybe (maybeToList)
import Language.Java.Syntax (ClassBody (..), ClassDecl (..), CompilationUnit (..), Decl (..), FormalParam (..), Ident (Ident), MemberDecl (..), Modifier (Final), TypeDecl (..), VarDeclId (VarId))

data Error = FuncVarNotFinal {func :: String, var :: String}
  deriving (Show)

check :: CompilationUnit -> [Error]
check (CompilationUnit _ _ classtype) = concatMap checkTypeDecl classtype

checkTypeDecl :: TypeDecl -> [Error]
checkTypeDecl (ClassTypeDecl classDecl) = checkClassType classDecl
checkTypeDecl (InterfaceTypeDecl _) = []

checkClassType :: ClassDecl -> [Error]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = concatMap checkDecl body
checkClassType (EnumDecl _ _ _ _) = []

checkDecl :: Decl -> [Error]
checkDecl (MemberDecl memberDecl) = checkMemberDecl memberDecl
checkDecl (InitDecl _ _) = []

checkMemberDecl :: MemberDecl -> [Error]
checkMemberDecl (MethodDecl _ _ _ (Ident ident) formalParam _ _ _) =
  concatMap
    ( \p ->
        let var = checkFormalParam p
         in map (\v -> FuncVarNotFinal {func = ident, var = v}) (maybeToList var)
    )
    formalParam
checkMemberDecl _ = []

checkFormalParam :: FormalParam -> Maybe String
checkFormalParam (FormalParam modifier _ _ (VarId (Ident n))) = if Final `elem` modifier then Nothing else Just n
