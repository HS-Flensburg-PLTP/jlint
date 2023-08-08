module Language.Java.Rules.CheckNonFinalMethodAttributes (check) where

import Data.List.Extra (none)
import Language.Java.AST (extractMethodParameters, extractVarName)
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  formalParam <- extractMethodParameters cUnit
  checkFormalParamList formalParam path

checkFormalParamList :: (String, [FormalParam Parsed]) -> FilePath -> [RDF.Diagnostic]
checkFormalParamList (_, formalParams) path = concatMap checkFormalParam formalParams
  where
    checkFormalParam (FormalParam _ modifier _ _ varid) =
      [ RDF.rangeDiagnostic
          "Language.Java.Rules.CheckNonFinalMethodAttributes"
          (extractVarName varid ++ " is not declared as Final")
          dummySourceSpan
          path
        | none Modifier.isFinal modifier
      ]
