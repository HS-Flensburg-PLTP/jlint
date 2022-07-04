module CheckNonFinalMethodAttributes (check) where

import AST (extractMethodParameters)
import Language.Java.Syntax (CompilationUnit (..), FormalParam (..), Ident (Ident), Modifier (Final), VarDeclId (VarDeclArray, VarId))
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  formalparam <- extractMethodParameters cUnit
  checkFormalParamList formalparam path

checkFormalParamList :: (String, [FormalParam]) -> FilePath -> [Diagnostic]
checkFormalParamList (methodName, formalparams) path = concatMap checkFormalParam formalparams
  where
    checkFormalParam (FormalParam [] _ _ varid) =
      return (methodDiagnostic methodName (varId varid ++ " is not declared as Final") path)
    checkFormalParam (FormalParam modifier _ _ varid) =
      if Final `notElem` modifier
        then return (methodDiagnostic methodName (varId varid ++ " is not declared as Final") path)
        else []
    varId (VarDeclArray varDeclId) = varId varDeclId
    varId (VarId (Ident n)) = n
