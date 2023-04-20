module Language.Java.Rules.CheckNonFinalMethodAttributes (check) where

import Language.Java.AST (extractMethodParameters, extractVarName)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  formalParam <- extractMethodParameters cUnit
  checkFormalParamList formalParam path

checkFormalParamList :: (String, [FormalParam]) -> FilePath -> [RDF.Diagnostic]
checkFormalParamList (_, formalParams) path = concatMap checkFormalParam formalParams
  where
    checkFormalParam (FormalParam modifier _ _ varid) =
      [ RDF.rangeDiagnostic
          "Language.Java.Rules.CheckNonFinalMethodAttributes"
          (extractVarName varid ++ " is not declared as Final")
          dummySourceSpan
          path
        | Final `notElem` modifier
      ]
