module Language.Java.Rules where

import Config
import Data.Map (Map, findWithDefault, fromList)
import qualified Language.Java.Rules.AvoidMultipleTopLevelDecl as AvoidMultipleTopLevelDecl
import qualified Language.Java.Rules.AvoidMultipleVarDecl as AvoidMultipleVarDecl
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified Language.Java.Rules.CheckNonFinalMethodAttributes as CheckNonFinalMethodAttributes
import qualified Language.Java.Rules.CheckNonPrivateAttributes as CheckNonPrivateAttributes
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified Language.Java.Rules.ConstantPropagation as ConstantPropagation
import qualified Language.Java.Rules.DeclarationOrder as DeclarationOrder
import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.ModifiedControlVariable as ModifiedControlVariable
import qualified Language.Java.Rules.NamingConventions as NamingConventions
import qualified Language.Java.Rules.NeedBraces as NeedBraces
import qualified Language.Java.Rules.NoLoopBreak as NoLoopBreak
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ProhibitAnnotations as ProhibitAnnotations
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
import qualified Language.Java.Rules.SimplifyBooleanReturn as SimplifyBooleanReturn
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import qualified Language.Java.Rules.UseIncrementDecrementOperator as UseIncrementDecrementOperator
import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import qualified Language.Java.Rules.UsePostIncrementDecrement as UsePostIncrementDecrement
import Language.Java.Syntax
import qualified RDF

annotationswhitelist :: [String]
annotationswhitelist = ["Override"]

checks :: [CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]]
checks =
  [ AvoidMultipleTopLevelDecl.check,
    AvoidMultipleVarDecl.check,
    AvoidNegations.check,
    AvoidStarImport.check,
    ConsistentOverrideEqualsHashCode.check,
    ConstantPropagation.check,
    InitializeVariables.check,
    ModifiedControlVariable.check,
    NoNullPointerExceptionsForControl.check,
    ParameterNumber.check,
    PreferExpressions.check,
    ReduceScope.check,
    RedundantModifiers.check,
    UseAssignOp.check,
    UseElse.check,
    DeclarationOrder.check,
    UseIncrementDecrementOperator.check,
    ProhibitAnnotations.check annotationswhitelist,
    UseJavaArrayTypeStyle.check
  ]

checkAll :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks

checkWithConfig :: [Config] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkWithConfig config cUnit path = concatMap (\f -> f cUnit path) (checkRule (extractRuleNames config))

checkRule :: [String] -> [CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]]
checkRule = map (\s -> findWithDefault (\_ _ -> []) s checkMapping)

checkMapping :: Map String (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
checkMapping =
  fromList
    [ ("AvoidMultipleTopLevelDecl", AvoidMultipleTopLevelDecl.check),
      ("AvoidMultipleVarDecl", AvoidMultipleVarDecl.check),
      ("AvoidNegations", AvoidNegations.check),
      ("AvoidStarImport", AvoidStarImport.check),
      ("CheckNonFinalMethodAttributes", CheckNonFinalMethodAttributes.check),
      ("CheckNonPrivateAttributes", CheckNonPrivateAttributes.check),
      ("ConsistentOverrideEqualsHashCode", ConsistentOverrideEqualsHashCode.check),
      ("ConstantPropagation", ConstantPropagation.check),
      ("DefaultComesLast", DefaultComesLast.check),
      ("InitializeVariables", InitializeVariables.check),
      ("NamingConventions", NamingConventions.check),
      ("NeedBraces", NeedBraces.check),
      ("NoLoopBreak", NoLoopBreak.check),
      ("NoNullPointerExceptionsForControl", NoNullPointerExceptionsForControl.check),
      ("ParameterNumber", ParameterNumber.check),
      ("PreferExpressions", PreferExpressions.check),
      ("ReduceScope", ReduceScope.check),
      ("RedundantModifiers", RedundantModifiers.check),
      ("SameExecutionsInIf", SameExecutionsInIf.check),
      ("SimplifyBooleanReturn", SimplifyBooleanReturn.check),
      ("UseAssignOp", UseAssignOp.check),
      ("UseElse", UseElse.check),
      ("UseIncrementDecrementOperator", UseIncrementDecrementOperator.check),
      ("UseJavaArrayTypeStyle", UseJavaArrayTypeStyle.check),
      ("UsePostIncrementDecrement", UsePostIncrementDecrement.check)
    ]

extractRuleNames :: [Config] -> [String]
extractRuleNames = map rule
