module Language.Java.Rules (defaultConfig, checkWithConfig) where

import Config (Rule (..))
import Control.Monad.Extra (concatMapM)
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.AvoidOuterNegations as AvoidOuterNegations
import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified Language.Java.Rules.CheckNonFinalMethodParameters as CheckNonFinalMethodParameters
import qualified Language.Java.Rules.CheckNonPrivateAttributes as CheckNonPrivateAttributes
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified Language.Java.Rules.DeclarationOrder as DeclarationOrder
import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
import qualified Language.Java.Rules.ExplicitValue as ExplicitValue
import qualified Language.Java.Rules.InitializeVariables as InitializeVariables
import qualified Language.Java.Rules.MethodInvNumber as MethodInvNumber
import qualified Language.Java.Rules.MethodNames as MethodNames
import qualified Language.Java.Rules.ModifiedControlVariable as ModifiedControlVariable
import qualified Language.Java.Rules.MultipleStringLiterals as MultipleStringLiterals
import qualified Language.Java.Rules.MultipleVariableDeclarations as MultipleVariableDeclarations
import qualified Language.Java.Rules.NamingConventions as NamingConventions
import qualified Language.Java.Rules.NeedBraces as NeedBraces
import qualified Language.Java.Rules.NoAnnotations as NoAnnotations
import qualified Language.Java.Rules.NoCasts as NoCasts
import qualified Language.Java.Rules.NoDummyNames as NoDummyNames
import qualified Language.Java.Rules.NoFurtherDataStructures as NoFurtherDataStructures
import qualified Language.Java.Rules.NoGermanNames as NoGermanNames
import qualified Language.Java.Rules.NoLoopBreak as NoLoopBreak
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.NoPostIncDecInExpression as NoPostIncDecInExpression
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
import qualified Language.Java.Rules.SimplifyBoolean as SimplifyBoolean
import qualified Language.Java.Rules.SingleTopLevelClass as SingleTopLevelClass
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
    CheckNonFinalMethodParameters,
    CheckNonPrivateAttributes,
    ConsistentOverrideEqualsHashCode,
    DeclarationOrder,
    DefaultComesLast,
    ExplicitValue,
    InitializeVariables,
    -- MethodInvNumber
    -- MethodNames,
    ModifiedControlVariable,
    MultipleStringLiterals,
    MultipleVariableDeclarations,
    NamingConventions,
    NeedBraces,
    NoAnnotations ["Override"],
    -- NoCasts
    NoDummyNames,
    -- NoFurtherDataStructures
    NoGermanNames,
    NoLoopBreak,
    NoNullPointerExceptionsForControl,
    NoPostIncDecInExpression,
    ParameterNumber Nothing,
    -- PredictMethodNames
    PreferExpressions,
    ReduceScope,
    RedundantModifiers,
    SameExecutionsInIf,
    SimplifyBoolean,
    SingleTopLevelClass,
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
checkFromConfig CheckNonFinalMethodParameters = liftIO CheckNonFinalMethodParameters.check
checkFromConfig CheckNonPrivateAttributes = liftIO CheckNonPrivateAttributes.check
checkFromConfig ConsistentOverrideEqualsHashCode = liftIO ConsistentOverrideEqualsHashCode.check
checkFromConfig DeclarationOrder = liftIO DeclarationOrder.check
checkFromConfig DefaultComesLast = liftIO DefaultComesLast.check
checkFromConfig ExplicitValue = liftIO ExplicitValue.check
checkFromConfig InitializeVariables = liftIO InitializeVariables.check
checkFromConfig (MethodInvNumber called limited maxInv) = liftIO (MethodInvNumber.check called limited maxInv)
checkFromConfig (MethodNames methods) = MethodNames.check methods
checkFromConfig ModifiedControlVariable = liftIO ModifiedControlVariable.check
checkFromConfig MultipleStringLiterals = liftIO MultipleStringLiterals.check
checkFromConfig MultipleVariableDeclarations = liftIO MultipleVariableDeclarations.check
checkFromConfig NamingConventions = liftIO NamingConventions.check
checkFromConfig NeedBraces = liftIO NeedBraces.check
checkFromConfig (NoAnnotations whitelist) = liftIO (NoAnnotations.check whitelist)
checkFromConfig (NoCasts whitelist) = liftIO (NoCasts.check whitelist)
checkFromConfig NoDummyNames = liftIO NoDummyNames.check
checkFromConfig (NoFurtherDataStructures methodNames) = liftIO (NoFurtherDataStructures.check methodNames)
checkFromConfig NoGermanNames = NoGermanNames.check
checkFromConfig NoLoopBreak = liftIO NoLoopBreak.check
checkFromConfig NoNullPointerExceptionsForControl = liftIO NoNullPointerExceptionsForControl.check
checkFromConfig NoPostIncDecInExpression = liftIO NoPostIncDecInExpression.check
checkFromConfig (ParameterNumber maybeMax) = liftIO (ParameterNumber.check maybeMax)
checkFromConfig PreferExpressions = liftIO PreferExpressions.check
checkFromConfig ReduceScope = liftIO ReduceScope.check
checkFromConfig RedundantModifiers = liftIO RedundantModifiers.check
checkFromConfig SameExecutionsInIf = liftIO SameExecutionsInIf.check
checkFromConfig SimplifyBoolean = liftIO SimplifyBoolean.check
checkFromConfig SingleTopLevelClass = liftIO SingleTopLevelClass.check
checkFromConfig UseAssignOp = liftIO UseAssignOp.check
checkFromConfig UseElse = liftIO UseElse.check
checkFromConfig UseIncrementDecrementOperator = liftIO UseIncrementDecrementOperator.check
checkFromConfig UseJavaArrayTypeStyle = liftIO UseJavaArrayTypeStyle.check
checkFromConfig UsePostIncrementDecrement = liftIO UsePostIncrementDecrement.check

liftIO :: (a -> b -> c) -> a -> b -> IO c
liftIO f a b = pure (f a b)
