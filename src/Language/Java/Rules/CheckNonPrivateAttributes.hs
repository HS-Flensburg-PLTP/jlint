module Language.Java.Rules.CheckNonPrivateAttributes (check) where

import Language.Java.AST (extractAttributes)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  attributes <- extractAttributes cUnit
  checkAttributes attributes path

checkAttributes :: ([String], [Modifier]) -> FilePath -> [RDF.Diagnostic]
checkAttributes (varNames, mods) path =
  concatMap (checkModifier mods) varNames
  where
    checkModifier modifier varname =
      [ RDF.rangeDiagnostic
          "Language.Java.Rules.CheckNonPrivateAttributes"
          (varname ++ " is not declared as private")
          dummySourceSpan
          path
        | Private `notElem` modifier
      ]
