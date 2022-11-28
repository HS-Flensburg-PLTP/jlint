module Language.Java.Rules where

-- import Language.Java.Rules.CheckNonFinalMethodAttributes
-- import Language.Java.Rules.CheckNonPrivateAttributes
-- import Language.Java.Rules.CheckScope
-- import Language.Java.Rules.DefaultComesLast
-- import Language.Java.Rules.EmptyLoopBody

-- import Language.Java.Rules.NamingConventions
-- import Language.Java.Rules.NeedBraces
-- import Language.Java.Rules.NoLoopBreak
-- import Language.Java.Rules.NoNegation

-- import Language.Java.Rules.SameExecutionsInIf
-- import Language.Java.Rules.SimplifyBooleanReturn
-- import Language.Java.Rules.UnnecessaryVariables
-- import Language.Java.Rules.UnusedLocalVariable

import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import Language.Java.Rules.UseElse as UseElse (check)
import Language.Java.Syntax (CompilationUnit)
import RDF (Diagnostic)

checks :: [CompilationUnit -> FilePath -> [Diagnostic]]
checks =
  [ AvoidNegations.check,
    InitializeVariables.check,
    NoNullPointerExceptionsForControl.check,
    PreferExpressions.check,
    ReduceScope.check,
    UseElse.check
  ]

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
--   SameExecutionsInIf.check,
--   UnusedLocalVariable.checkMethodVars
-- ]

checkAll :: CompilationUnit -> FilePath -> [Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks
