module CheckNonFinalMethodAttributes (check) where

import AST (extractMethodParameters, extractVarName)
import Language.Java.Syntax (CompilationUnit (..), FormalParam (..), Modifier (Final))
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  formalParam <- extractMethodParameters cUnit
  checkFormalParamList formalParam path

checkFormalParamList :: (String, [FormalParam]) -> FilePath -> [Diagnostic]
checkFormalParamList (methodName, formalParams) path = concatMap checkFormalParam formalParams
  where
    checkFormalParam (FormalParam modifier _ _ varid) =
      [methodDiagnostic methodName (extractVarName varid ++ " is not declared as Final") path | Final `notElem` modifier]