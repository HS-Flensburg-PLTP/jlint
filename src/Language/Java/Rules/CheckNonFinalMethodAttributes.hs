module Language.Java.Rules.CheckNonFinalMethodAttributes (check) where

import Data.List.Extra (none)
import Language.Java.AST (extractMethodParameters, extractVarName)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  formalParam <- extractMethodParameters cUnit
  checkFormalParamList formalParam path

checkFormalParamList :: (String, [FormalParam]) -> FilePath -> [RDF.Diagnostic]
checkFormalParamList (_, formalParams) path = concatMap checkFormalParam formalParams
  where
    checkFormalParam (FormalParam _ modifier _ _ varid) =
      [ RDF.rangeDiagnostic
          "Language.Java.Rules.CheckNonFinalMethodAttributes"
          (extractVarName varid ++ " is not declared as Final")
          dummySourceSpan
          path
        | none (eq IgnoreSourceSpan Final) modifier
      ]
