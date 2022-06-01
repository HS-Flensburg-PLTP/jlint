module CheckNonFinalMethodAttributes (check) where

import Data.Maybe (maybeToList)
import Language.Java.Syntax (ClassBody (..), ClassDecl (..), CompilationUnit (..), Decl (..), FormalParam (..), Ident (Ident), MemberDecl (..), Modifier (Final), TypeDecl (..), VarDeclId (VarId))
import RDF (Diagnostic (..), Location (..))

data Error = FuncVarNotFinal {func :: String, var :: String}
  deriving (Show)

check :: CompilationUnit -> [Diagnostic]
check (CompilationUnit _ _ classtype) = concatMap checkTypeDecl classtype

checkTypeDecl :: TypeDecl -> [Diagnostic]
checkTypeDecl (ClassTypeDecl classDecl) = checkClassType classDecl
checkTypeDecl (InterfaceTypeDecl _) = []

checkClassType :: ClassDecl -> [Diagnostic]
checkClassType (ClassDecl _ _ _ _ _ (ClassBody body)) = concatMap checkDecl body
checkClassType (EnumDecl _ _ _ _) = []

checkDecl :: Decl -> [Diagnostic]
checkDecl (MemberDecl memberDecl) = checkMemberDecl memberDecl
checkDecl (InitDecl _ _) = []

checkMemberDecl :: MemberDecl -> [Diagnostic]
checkMemberDecl (MethodDecl _ _ _ (Ident ident) formalParam _ _ _) =
  concatMap
    ( \p ->
        let var = checkFormalParam p
         in --  in maybeToList (fmap (\v -> FuncVarNotFinal {func = ident, var = v}) var)
            maybeToList
              ( fmap
                  ( \v ->
                      Diagnostic
                        { message = "Argument " ++ v ++ " in " ++ ident ++ " is not declared as 'final'.",
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
                  )
                  var
              )
    )
    formalParam
checkMemberDecl _ = []

checkFormalParam :: FormalParam -> Maybe String
checkFormalParam (FormalParam modifier _ _ (VarId (Ident n))) = if Final `elem` modifier then Nothing else Just n
