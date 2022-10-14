module Rules where

import CheckNonFinalMethodAttributes
import CheckNonPrivateAttributes
import CheckScope
import DefaultComesLast
import EmptyLoopBody
import Language.Java.Syntax
import NamingConventions
import NeedBraces
import NoLoopBreak
import NoNegation
import RDF
import SameExecutionsInIf
import SimplifyBooleanReturn
import UnnecessaryVariables
import UnusedLocalVariable
import UseElse

checks :: [CompilationUnit -> FilePath -> [Diagnostic]]
checks = [UseElse.check]

-- [ NeedBraces.check,
--   CheckNonFinalMethodAttributes.check,
--   CheckNonPrivateAttributes.check,
--   EmptyLoopBody.check,
--   SimplifyBooleanReturn.check,
--   NoLoopBreak.check,
--   UnnecessaryVariables.checkMethodVars,
--   NeedBraces.check,
--   CheckScope.check,
--   DefaultComesLast.check,
--   NamingConventions.checkPackageName,
--   NamingConventions.checkMethodName,
--   NamingConventions.checkParameterName,
--   NamingConventions.checkStaticVariableName,
--   NamingConventions.checkLocalName,
--   NamingConventions.checkMemberName,
--   NamingConventions.checkTypeName,
--   NoNegation.check,
--   SameExecutionsInIf.check,
--   UnusedLocalVariable.checkMethodVars
-- ]

checkAll :: CompilationUnit -> FilePath -> [Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks
