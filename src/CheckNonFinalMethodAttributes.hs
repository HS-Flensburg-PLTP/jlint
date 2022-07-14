module CheckNonFinalMethodAttributes (check) where

import AST (extractMethodParameters)
import Language.Java.Syntax (CompilationUnit (..), FormalParam (..), Ident (Ident), Modifier (Final), VarDeclId (VarDeclArray, VarId))
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  formalParam <- extractMethodParameters cUnit
  checkFormalParamList formalParam path

checkFormalParamList :: (String, [FormalParam]) -> FilePath -> [Diagnostic]
checkFormalParamList (methodName, formalParams) path = concatMap checkFormalParam formalParams
  where
    checkFormalParam (FormalParam [] _ _ varid) =
      return (methodDiagnostic methodName (extractVarName varid ++ " is not declared as Final") path)
    checkFormalParam (FormalParam modifier _ _ varid) =
      if Final `notElem` modifier
        then return (methodDiagnostic methodName (extractVarName varid ++ " is not declared as Final") path)
        else []
    extractVarName (VarDeclArray varDeclId) = extractVarName varDeclId
    extractVarName (VarId (Ident n)) = n
