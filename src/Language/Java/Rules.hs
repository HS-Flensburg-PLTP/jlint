module Language.Java.Rules where

import Config (Rule (..))
import qualified Language.Java.Rules.AvoidMultipleTopLevelDecl as AvoidMultipleTopLevelDecl
import qualified Language.Java.Rules.AvoidMultipleVarDecl as AvoidMultipleVarDecl
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.AvoidOuterNegations as AvoidOuterNegations
import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified Language.Java.Rules.CheckNonFinalMethodAttributes as CheckNonFinalMethodAttributes
import qualified Language.Java.Rules.CheckNonPrivateAttributes as CheckNonPrivateAttributes
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified Language.Java.Rules.DeclarationOrder as DeclarationOrder
import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
import qualified Language.Java.Rules.ExplicitValue as ExplicitValue
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.ModifiedControlVariable as ModifiedControlVariable
import qualified Language.Java.Rules.NamingConventions as NamingConventions
import qualified Language.Java.Rules.NeedBraces as NeedBraces
import qualified Language.Java.Rules.NoLoopBreak as NoLoopBreak
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.PredictMethodNames as PredictMethodNames
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ProhibitAnnotations as ProhibitAnnotations
import qualified Language.Java.Rules.ProhibitGermanNames as ProhibitGermanNames
import qualified Language.Java.Rules.ProhibitMyIdentPrefix as ProhibitMyIdentPrefix
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
import qualified Language.Java.Rules.SimplifyBooleanReturn as SimplifyBooleanReturn
import qualified Language.Java.Rules.UnusedLocalVariable as UnusedLocalVariable
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import qualified Language.Java.Rules.UseIncrementDecrementOperator as UseIncrementDecrementOperator
import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import qualified Language.Java.Rules.UsePostIncrementDecrement as UsePostIncrementDecrement
import Language.Java.Syntax
import qualified RDF

checks :: [CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]]
checks =
  [ AvoidMultipleTopLevelDecl.check,
    AvoidMultipleVarDecl.check,
    AvoidNegations.check,
    AvoidOuterNegations.check,
    AvoidStarImport.check,
    ConsistentOverrideEqualsHashCode.check,
    DeclarationOrder.check,
    ExplicitValue.check,
    InitializeVariables.check,
    ModifiedControlVariable.check,
    NoNullPointerExceptionsForControl.check,
    ParameterNumber.check Nothing,
    PreferExpressions.check,
    ProhibitAnnotations.checkWithDefaultValue,
    ProhibitMyIdentPrefix.check,
    ReduceScope.check,
    RedundantModifiers.check,
    UseAssignOp.check,
    UseElse.check,
    UseIncrementDecrementOperator.check,
    UseJavaArrayTypeStyle.check
  ]

checksIO :: [CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]]
checksIO =
  [ ProhibitGermanNames.check,
    PredictMethodNames.check
  ]

checkAll :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks

checkAllIO :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
checkAllIO = executeAll checksIO

executeAll :: [CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]] -> CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
executeAll [] _ _ = return []
executeAll (check : checks) cUnit path = do
  result <- check cUnit path
  results <- executeAll checks cUnit path
  return (result ++ results)

checkWithConfig :: [Config] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkWithConfig config cUnit path = concatMap (\f -> f cUnit path) (checkRule (extractRuleNames config))

checkWithConfig :: [Rule] -> (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
checkWithConfig config cUnit path = concatMap (\f -> f cUnit path) (checkRule config)

checkRule :: [Rule] -> [CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]]
checkRule = map checkFromConfig

checkFromConfig :: Rule -> (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
checkFromConfig AvoidMultipleTopLevelDecl = AvoidMultipleTopLevelDecl.check
checkFromConfig AvoidMultipleVarDecl = AvoidMultipleVarDecl.check
checkFromConfig AvoidNegations = AvoidNegations.check
checkFromConfig AvoidStarImport = AvoidStarImport.check
checkFromConfig CheckNonFinalMethodAttributes = CheckNonFinalMethodAttributes.check
checkFromConfig CheckNonPrivateAttributes = CheckNonPrivateAttributes.check
checkFromConfig ConsistentOverrideEqualsHashCode = ConsistentOverrideEqualsHashCode.check
checkFromConfig DeclarationOrder = DeclarationOrder.check
checkFromConfig DefaultComesLast = DefaultComesLast.check
checkFromConfig InitializeVariables = InitializeVariables.check
checkFromConfig ModifiedControlVariable = ModifiedControlVariable.check
checkFromConfig NamingConventions = NamingConventions.check
checkFromConfig NeedBraces = NeedBraces.check
checkFromConfig NoLoopBreak = NoLoopBreak.check
checkFromConfig NoNullPointerExceptionsForControl = NoNullPointerExceptionsForControl.check
checkFromConfig (ParameterNumber max) = ParameterNumber.check max
checkFromConfig PreferExpressions = PreferExpressions.check
checkFromConfig (ProhibitAnnotations whitelist) = ProhibitAnnotations.check whitelist
checkFromConfig ProhibitMyIdentPrefix = ProhibitMyIdentPrefix.check
checkFromConfig ReduceScope = ReduceScope.check
checkFromConfig RedundantModifiers = RedundantModifiers.check
checkFromConfig SameExecutionsInIf = SameExecutionsInIf.check
checkFromConfig SimplifyBooleanReturn = SimplifyBooleanReturn.check
checkFromConfig UnusedLocalVariable = UnusedLocalVariable.checkMethodVars
checkFromConfig UseAssignOp = UseAssignOp.check
checkFromConfig UseElse = UseElse.check
checkFromConfig UseIncrementDecrementOperator = UseIncrementDecrementOperator.check
checkFromConfig UseJavaArrayTypeStyle = UseJavaArrayTypeStyle.check
checkFromConfig UsePostIncrementDecrement = UsePostIncrementDecrement.check
