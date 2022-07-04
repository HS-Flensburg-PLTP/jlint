module CheckNonFinalMethodAttributes (check) where

import AST (extractMethodParameters)
import Language.Java.Syntax (CompilationUnit (..), FormalParam (..), Ident (Ident), Modifier (Final), VarDeclId (VarDeclArray, VarId))
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  formalparam <- extractMethodParameters cUnit
  checkFormalParamList formalparam path

checkFormalParamList :: (String, [FormalParam]) -> FilePath -> [Diagnostic]
checkFormalParamList (methodName, formalparams) path = checkFormalParam formalparams
  where
    checkFormalParam [] = []
    checkFormalParam (x : xs) = checkFParam x ++ checkFormalParam xs
    checkFParam (FormalParam [] _ _ varid) =
      return (methodDiagnostic methodName (varId varid ++ " is not declared as Final") path)
    checkFParam (FormalParam modifier _ _ varid) =
      if Final `notElem` modifier
        then return (methodDiagnostic methodName (varId varid ++ " is not declared as Final") path)
        else []
    varId (VarDeclArray varDeclId) = varId varDeclId
    varId (VarId (Ident n)) = n
