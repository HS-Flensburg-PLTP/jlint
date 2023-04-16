module Language.Java.Rules where

import qualified Language.Java.Rules.AvoidMultipleVarDecl as AvoidMultipleVarDecl
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ProhibitAnnotations as ProhibitAnnotations
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import Language.Java.Syntax (CompilationUnit)
import qualified RDF

whitelist :: [String]
whitelist = ["Override"]

checks :: [CompilationUnit -> FilePath -> [RDF.Diagnostic]]
checks =
  [ AvoidMultipleVarDecl.check,
    AvoidNegations.check,
    InitializeVariables.check,
    NoNullPointerExceptionsForControl.check,
    ParameterNumber.check,
    PreferExpressions.check,
    ReduceScope.check,
    RedundantModifiers.check,
    UseAssignOp.check,
    UseElse.check,
    ProhibitAnnotations.check whitelist,
    UseJavaArrayTypeStyle.check,
    ConsistentOverrideEqualsHashCode.check
  ]

checkAll :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks
