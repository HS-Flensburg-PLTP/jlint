module Language.Java.Rules (defaultConfig, checkWithConfig) where

import Config (Rule (..))
import Control.Monad.Extra (concatMapM)
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.AvoidOuterNegations as AvoidOuterNegations
import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified Language.Java.Rules.DeclarationOrder as DeclarationOrder
import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
import qualified Language.Java.Rules.DuplicateInConditional as DuplicateInConditional
import qualified Language.Java.Rules.Evaluation as Evaluation
import qualified Language.Java.Rules.ExplicitValue as ExplicitValue
import qualified Language.Java.Rules.FinalParameters as FinalParameters
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.MethodInvocations as MethodInvocations
import qualified Language.Java.Rules.MethodNames as MethodNames
import qualified Language.Java.Rules.ModifiedControlVariable as ModifiedControlVariable
import qualified Language.Java.Rules.MultipleStringLiterals as MultipleStringLiterals
import qualified Language.Java.Rules.MultipleVariableDeclarations as MultipleVariableDeclarations
import qualified Language.Java.Rules.NamingConventions as NamingConventions
import qualified Language.Java.Rules.NeedBraces as NeedBraces
import qualified Language.Java.Rules.NoAnnotations as NoAnnotations
import qualified Language.Java.Rules.NoCasts as NoCasts
import qualified Language.Java.Rules.NoDummyNames as NoDummyNames
import qualified Language.Java.Rules.NoExtraDataStructures as NoExtraDataStructures
import qualified Language.Java.Rules.NoGermanNames as NoGermanNames
import qualified Language.Java.Rules.NoIncDecInExpression as NoIncDecInExpression
import qualified Language.Java.Rules.NoLoopBreak as NoLoopBreak
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.Pattern as Pattern
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.PrivateAttributes as PrivateAttributes
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantLocalVariable as RedundantLocalVariable
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
import qualified Language.Java.Rules.Simplify as Simplify
import qualified Language.Java.Rules.SimplifyBoolean as SimplifyBoolean
import qualified Language.Java.Rules.SingleTopLevelClass as SingleTopLevelClass
import qualified Language.Java.Rules.SuppressWarnings as SuppressWarnings
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import qualified Language.Java.Rules.UseIncrementDecrementOperator as UseIncrementDecrementOperator
import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import qualified Language.Java.Rules.UsePostIncrementDecrement as UsePostIncrementDecrement
import Language.Java.Syntax (CompilationUnit, Parsed)
import qualified RDF

defaultConfig :: [Rule]
defaultConfig =
  [ AvoidNegations,
    AvoidOuterNegations,
    AvoidStarImport,
    ConsistentOverrideEqualsHashCode,
    DeclarationOrder,
    DefaultComesLast,
    DuplicateInConditional,
    Evaluation,
    ExplicitValue,
    FinalParameters,
    InitializeVariables,
    -- MethodInvocations
    -- MethodNames,
    ModifiedControlVariable,
    MultipleStringLiterals,
    MultipleVariableDeclarations,
    NamingConventions,
    NeedBraces,
    NoAnnotations ["Override"],
    -- NoCasts
    NoDummyNames,
    -- NoExtraDataStructures
    NoGermanNames,
    NoIncDecInExpression,
    NoLoopBreak,
    NoNullPointerExceptionsForControl,
    ParameterNumber Nothing,
    -- PredictMethodNames
    PreferExpressions,
    PrivateAttributes,
    ReduceScope,
    RedundantLocalVariable (Just True),
    RedundantModifiers,
    SameExecutionsInIf,
    Simplify,
    SimplifyBoolean,
    SingleTopLevelClass,
    SuppressWarnings [],
    UseAssignOp,
    UseElse,
    UseIncrementDecrementOperator,
    UseJavaArrayTypeStyle,
    UsePostIncrementDecrement
  ]

checkWithConfig :: [Rule] -> CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
checkWithConfig rules cUnit filePath = concatMapM (\rule -> checkFromConfig rule cUnit filePath) rules

checkFromConfig :: Rule -> CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
checkFromConfig AvoidNegations = liftIO AvoidNegations.check
checkFromConfig AvoidOuterNegations = liftIO AvoidOuterNegations.check
checkFromConfig AvoidStarImport = liftIO AvoidStarImport.check
checkFromConfig ConsistentOverrideEqualsHashCode = liftIO ConsistentOverrideEqualsHashCode.check
checkFromConfig DeclarationOrder = liftIO DeclarationOrder.check
checkFromConfig DefaultComesLast = liftIO DefaultComesLast.check
checkFromConfig DuplicateInConditional = liftIO DuplicateInConditional.check
checkFromConfig ExplicitValue = liftIO ExplicitValue.check
checkFromConfig Evaluation = liftIO Evaluation.check
checkFromConfig FinalParameters = liftIO FinalParameters.check
checkFromConfig InitializeVariables = liftIO InitializeVariables.check
checkFromConfig (MethodInvocations called limited maxInv explanation) =
  liftIO (MethodInvocations.check called limited maxInv explanation)
checkFromConfig (MethodNames methods) = MethodNames.check methods
checkFromConfig ModifiedControlVariable = liftIO ModifiedControlVariable.check
checkFromConfig MultipleStringLiterals = liftIO MultipleStringLiterals.check
checkFromConfig MultipleVariableDeclarations = liftIO MultipleVariableDeclarations.check
checkFromConfig NamingConventions = liftIO NamingConventions.check
checkFromConfig NeedBraces = liftIO NeedBraces.check
checkFromConfig (NoAnnotations whitelist) = liftIO (NoAnnotations.check whitelist)
checkFromConfig (NoCasts whitelist) = liftIO (NoCasts.check whitelist)
checkFromConfig NoDummyNames = liftIO NoDummyNames.check
checkFromConfig (NoExtraDataStructures methodNames) = liftIO (NoExtraDataStructures.check methodNames)
checkFromConfig NoGermanNames = NoGermanNames.check
checkFromConfig NoIncDecInExpression = liftIO NoIncDecInExpression.check
checkFromConfig NoLoopBreak = liftIO NoLoopBreak.check
checkFromConfig NoNullPointerExceptionsForControl = liftIO NoNullPointerExceptionsForControl.check
checkFromConfig (ParameterNumber maybeMax) = liftIO (ParameterNumber.check maybeMax)
checkFromConfig (Pattern pattern_ explanation) = liftIO (Pattern.check pattern_ explanation)
checkFromConfig PreferExpressions = liftIO PreferExpressions.check
checkFromConfig PrivateAttributes = liftIO PrivateAttributes.check
checkFromConfig ReduceScope = liftIO ReduceScope.check
checkFromConfig (RedundantLocalVariable maybeActivated) = liftIO (RedundantLocalVariable.check maybeActivated)
checkFromConfig RedundantModifiers = liftIO RedundantModifiers.check
checkFromConfig SameExecutionsInIf = liftIO SameExecutionsInIf.check
checkFromConfig Simplify = liftIO Simplify.check
checkFromConfig SimplifyBoolean = liftIO SimplifyBoolean.check
checkFromConfig SingleTopLevelClass = liftIO SingleTopLevelClass.check
checkFromConfig (SuppressWarnings whitelist) = liftIO (SuppressWarnings.check whitelist)
checkFromConfig UseAssignOp = liftIO UseAssignOp.check
checkFromConfig UseElse = liftIO UseElse.check
checkFromConfig UseIncrementDecrementOperator = liftIO UseIncrementDecrementOperator.check
checkFromConfig UseJavaArrayTypeStyle = liftIO UseJavaArrayTypeStyle.check
checkFromConfig UsePostIncrementDecrement = liftIO UsePostIncrementDecrement.check

liftIO :: (a -> b -> c) -> a -> b -> IO c
liftIO f a b = pure (f a b)
