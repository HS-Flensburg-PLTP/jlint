module CheckNonPrivateAttributes where

import AST (extractAttributes)
import Language.Java.Syntax (CompilationUnit (..), Modifier (Private))
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  attributes <- extractAttributes cUnit
  checkAttributes attributes path

checkAttributes :: ([String], [Modifier]) -> FilePath -> [Diagnostic]
checkAttributes (varnames, mods) path =
  concatMap (checkModifier mods) varnames
  where
    checkModifier modifier varname =
      if Private `notElem` modifier then return (methodDiagnostic varname "Is not declared as private" path) else []
