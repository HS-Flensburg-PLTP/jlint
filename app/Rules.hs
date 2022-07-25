module Rules where

import CheckNonFinalMethodAttributes
import CheckNonPrivateAttributes
import CheckScope
import DefaultComesLast
import EmptyLoopBody
import Language.Java.Syntax
import NamingConventions
import NeedBraces
import NoNegation
import RDF
import SameExecutionsInIf
import SimplifyBooleanReturn
import UnnecessaryVariables
import UnusedLocalVariable

checks :: [CompilationUnit -> FilePath -> [Diagnostic]]
checks =
  [ NeedBraces.check,
    CheckNonFinalMethodAttributes.check,
    CheckNonPrivateAttributes.check,
    EmptyLoopBody.check,
    SimplifyBooleanReturn.check,
    UnnecessaryVariables.checkMethodVars,
    NeedBraces.check,
    CheckScope.check,
    DefaultComesLast.check,
    NamingConventions.checkPackageName,
    NamingConventions.checkMethodName,
    NamingConventions.checkParameterName,
    NamingConventions.checkStaticVariableName,
    NamingConventions.checkLocalName,
    NamingConventions.checkMemberName,
    NamingConventions.checkTypeName,
    NoNegation.check,
    SameExecutionsInIf.check,
    UnusedLocalVariable.checkMethodVars
  ]

checkAll :: CompilationUnit -> FilePath -> [Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks
