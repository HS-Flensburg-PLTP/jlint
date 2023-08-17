module Language.Java.Rules where

import Config (Rule (..))
import Control.Monad.Extra (concatMapM)
import qualified Language.Java.Rules.AvoidMultipleTopLevelDecl as AvoidMultipleTopLevelDecl
import qualified Language.Java.Rules.AvoidMultipleVarDecl as AvoidMultipleVarDecl
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
import qualified Language.Java.Rules.ModifiedControlVariable as ModifiedControlVariable
import qualified Language.Java.Rules.NeedBraces as NeedBraces
import qualified Language.Java.Rules.NoCasts as NoCasts
import qualified Language.Java.Rules.NoFurtherDataStructures as NoFurtherDataStructures
import qualified Language.Java.Rules.NoLoopBreak as NoLoopBreak
import qualified Language.Java.Rules.NoNullPointerExceptionsForControl as NoNullPointerExceptionsForControl
import qualified Language.Java.Rules.NoPostIncDecInExpression as NoPostIncDecInExpression
import qualified Language.Java.Rules.ParameterNumber as ParameterNumber
import qualified Language.Java.Rules.PreferExpressions as PreferExpressions
import qualified Language.Java.Rules.ProhibitAnnotations as ProhibitAnnotations
import qualified Language.Java.Rules.ProhibitGermanNames as ProhibitGermanNames
import qualified Language.Java.Rules.ProhibitMyIdentPrefix as ProhibitMyIdentPrefix
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.SameExecutionsInIf as SameExecutionsInIf
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
    CheckNonFinalMethodParameters.check,
    CheckNonPrivateAttributes.check,
    ConsistentOverrideEqualsHashCode.check,
    DeclarationOrder.check,
    DefaultComesLast.check,
    ExplicitValue.check,
    InitializeVariables.check,
    ModifiedControlVariable.check,
    NeedBraces.check,
    NoNullPointerExceptionsForControl.check,
    NoPostIncDecInExpression.check,
    ParameterNumber.check Nothing,
    PreferExpressions.check,
    ProhibitAnnotations.checkWithDefaultValue,
    ProhibitMyIdentPrefix.check,
    ReduceScope.check,
    RedundantModifiers.check,
    UseAssignOp.check,
    UseElse.check,
    UseIncrementDecrementOperator.check,
    UseJavaArrayTypeStyle.check,
    UsePostIncrementDecrement.check
  ]

checksIO :: [CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]]
checksIO =
  [ ProhibitGermanNames.check
  ]

checkAll :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks

checkAllIO :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
checkAllIO cUnit path = concatMapM (\f -> f cUnit path) checksIO

checkWithConfig :: [Rule] -> (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
checkWithConfig config cUnit path = concatMap (\f -> f cUnit path) (checkRule config)

checkWithConfigIO :: [Rule] -> (CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic])
checkWithConfigIO rulesIO cUnit path = concatMapM (\f -> f cUnit path) (checkRuleIO rulesIO)

checkRule :: [Rule] -> [CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]]
checkRule = map checkFromConfig

checkRuleIO :: [Rule] -> [CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]]
checkRuleIO = map checkFromConfigIO

checkFromConfig :: Rule -> (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
checkFromConfig AvoidMultipleTopLevelDecl = AvoidMultipleTopLevelDecl.check
checkFromConfig AvoidMultipleVarDecl = AvoidMultipleVarDecl.check
checkFromConfig AvoidNegations = AvoidNegations.check
checkFromConfig AvoidOuterNegations = AvoidOuterNegations.check
checkFromConfig AvoidStarImport = AvoidStarImport.check
checkFromConfig CheckNonFinalMethodParameters = CheckNonFinalMethodParameters.check
checkFromConfig CheckNonPrivateAttributes = CheckNonPrivateAttributes.check
checkFromConfig ConsistentOverrideEqualsHashCode = ConsistentOverrideEqualsHashCode.check
checkFromConfig DeclarationOrder = DeclarationOrder.check
checkFromConfig DefaultComesLast = DefaultComesLast.check
checkFromConfig ExplicitValue = ExplicitValue.check
checkFromConfig InitializeVariables = InitializeVariables.check
checkFromConfig (MethodInvNumber called limited maxInv) = MethodInvNumber.check called limited maxInv
checkFromConfig ModifiedControlVariable = ModifiedControlVariable.check
checkFromConfig NeedBraces = NeedBraces.check
checkFromConfig (NoCasts whitelist) = NoCasts.check whitelist
checkFromConfig (NoFurtherDataStructures methodNames) = NoFurtherDataStructures.check methodNames
checkFromConfig NoLoopBreak = NoLoopBreak.check
checkFromConfig NoNullPointerExceptionsForControl = NoNullPointerExceptionsForControl.check
checkFromConfig NoPostIncDecInExpression = NoPostIncDecInExpression.check
checkFromConfig (ParameterNumber max) = ParameterNumber.check max
checkFromConfig PreferExpressions = PreferExpressions.check
checkFromConfig (ProhibitAnnotations whitelist) = ProhibitAnnotations.check whitelist
checkFromConfig ProhibitMyIdentPrefix = ProhibitMyIdentPrefix.check
checkFromConfig ReduceScope = ReduceScope.check
checkFromConfig RedundantModifiers = RedundantModifiers.check
checkFromConfig SameExecutionsInIf = SameExecutionsInIf.check
checkFromConfig UseAssignOp = UseAssignOp.check
checkFromConfig UseElse = UseElse.check
checkFromConfig UseIncrementDecrementOperator = UseIncrementDecrementOperator.check
checkFromConfig UseJavaArrayTypeStyle = UseJavaArrayTypeStyle.check
checkFromConfig UsePostIncrementDecrement = UsePostIncrementDecrement.check
checkFromConfig _ = mempty

checkFromConfigIO :: Rule -> (CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic])
checkFromConfigIO ProhibitGermanNames = ProhibitGermanNames.check
checkFromConfigIO _ = mempty
