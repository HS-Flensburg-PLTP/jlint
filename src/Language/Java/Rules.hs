module Language.Java.Rules where

import Config (Rule (..))
import qualified Language.Java.Rules.AvoidMultipleTopLevelDecl as AvoidMultipleTopLevelDecl
import qualified Language.Java.Rules.AvoidMultipleVarDecl as AvoidMultipleVarDecl
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified Language.Java.Rules.DeclarationOrder as DeclarationOrder
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.ModifiedControlVariable as ModifiedControlVariable
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ProhibitAnnotations as ProhibitAnnotations
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import qualified Language.Java.Rules.UseIncrementDecrementOperator as UseIncrementDecrementOperator
import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import Language.Java.Syntax
import qualified RDF

checks :: [CompilationUnit -> FilePath -> [RDF.Diagnostic]]
checks =
  [ AvoidMultipleTopLevelDecl.check,
    AvoidMultipleVarDecl.check,
    AvoidNegations.check,
    AvoidStarImport.check,
    ConsistentOverrideEqualsHashCode.check,
    InitializeVariables.check,
    ModifiedControlVariable.check,
    NoNullPointerExceptionsForControl.check,
    ParameterNumber.checkWithDefaultValue,
    PreferExpressions.check,
    ProhibitAnnotations.checkWithDefaultValue,
    ReduceScope.check,
    RedundantModifiers.check,
    UseAssignOp.check,
    UseElse.check,
    DeclarationOrder.check,
    UseIncrementDecrementOperator.check,
    UseJavaArrayTypeStyle.check
  ]

checkAll :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks

checkWithConfig :: [Rule] -> (CompilationUnit -> FilePath -> [RDF.Diagnostic])
checkWithConfig config cUnit path = concatMap (\f -> f cUnit path) (checkRule config)

checkRule :: [Rule] -> [CompilationUnit -> FilePath -> [RDF.Diagnostic]]
checkRule = map checkFromConfig

checkFromConfig :: Rule -> (CompilationUnit -> FilePath -> [RDF.Diagnostic])
checkFromConfig (ParameterNumber max min) = ParameterNumber.check (ParameterNumber max min)
checkFromConfig (ProhibitAnnotations whitelist) = ProhibitAnnotations.check (ProhibitAnnotations whitelist)
