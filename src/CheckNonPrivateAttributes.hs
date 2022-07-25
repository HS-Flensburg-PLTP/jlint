module CheckNonPrivateAttributes where

import AST (extractAttributes)
import Language.Java.Syntax (CompilationUnit (..), Modifier (Private))
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  attributes <- extractAttributes cUnit
  checkAttributes attributes path

checkAttributes :: ([String], [Modifier]) -> FilePath -> [Diagnostic]
checkAttributes (varNames, mods) path =
  concatMap (checkModifier mods) varNames
  where
    checkModifier modifier varname =
      [methodDiagnostic varname "Is not declared as private" path | Private `notElem` modifier]
      