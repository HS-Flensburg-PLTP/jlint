module Language.Java.Rules where

import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF

checks :: [CompilationUnit -> FilePath -> [RDF.Diagnostic]]
checks =
  [ AvoidNegations.check,
    InitializeVariables.check,
    NoNullPointerExceptionsForControl.check,
    PreferExpressions.check,
    ReduceScope.check,
    ParameterNumber.check,
    UseAssignOp.check,
    UseElse.check
  ]

checkAll :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks
