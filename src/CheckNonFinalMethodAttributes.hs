module CheckNonFinalMethodAttributes (check) where

import Data.Maybe (maybeToList)
import Language.Java.Syntax (ClassBody (..), ClassDecl (..), CompilationUnit (..), Decl (..), FormalParam (..), Ident (Ident), MemberDecl (..), Modifier (Final), TypeDecl (..), VarDeclId (VarId))
import RDF (Diagnostic (..), Location (..))

data Error = FuncVarNotFinal {func :: String, var :: String}
  deriving (Show)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check (CompilationUnit _ _ classtype) = checkTypeDecls classtype

checkTypeDecls :: [TypeDecl] -> FilePath -> [Diagnostic]
checkTypeDecls [] _ = []
checkTypeDecls (x:xs) path = checkTypeDecl x path ++ checkTypeDecls xs path

checkTypeDecl :: TypeDecl -> FilePath -> [Diagnostic]
checkTypeDecl (ClassTypeDecl classDecl) path = checkClassType classDecl path
checkTypeDecl (InterfaceTypeDecl _) _ = []

checkClassType :: ClassDecl -> FilePath -> [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) path = checkDecls body path
checkClassType (EnumDecl {}) _ = []

checkDecls :: [Decl] -> FilePath -> [Diagnostic]
checkDecls [] _ = []
checkDecls (x:xs) path = checkDecl x path ++ checkDecls xs path

checkDecl :: Decl -> FilePath -> [Diagnostic]
checkDecl (MemberDecl memberDecl) path = checkMemberDecl memberDecl path
checkDecl (InitDecl _ _) _ = []

checkMemberDecl :: MemberDecl -> FilePath -> [Diagnostic]
checkMemberDecl (MethodDecl _ _ _ (Ident ident) formalParam _ _ _) path =
  concatMap
    ( \p ->
        let var = checkFormalParam p
         in maybeToList
              ( fmap
                  ( \v ->
                      Diagnostic
                        { message = "Argument " ++ v ++ " in " ++ ident ++ " is not declared as 'final'.",
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
                  )
                  var
              )
    )
    formalParam
checkMemberDecl _ _ = []

checkFormalParam :: FormalParam -> Maybe String
checkFormalParam (FormalParam modifier _ _ (VarId (Ident n))) = if Final `elem` modifier then Nothing else Just n
