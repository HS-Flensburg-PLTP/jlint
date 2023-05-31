module Language.Java.Rules where

import Config (Config (Config), ParameterNumberConfig (ParameterNumberConfig), ProhibitAnnotationsConfig (ProhibitAnnotationsConfig))
import qualified Language.Java.Rules.AvoidMultipleTopLevelDecl as AvoidMultipleTopLevelDecl
import qualified Language.Java.Rules.AvoidMultipleVarDecl as AvoidMultipleVarDecl
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified Language.Java.Rules.CheckNonFinalMethodAttributes as CheckNonFinalMethodAttributes
import qualified Language.Java.Rules.CheckNonPrivateAttributes as CheckNonPrivateAttributes
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
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
    ParameterNumber.check (ParameterNumberConfig (Just 7)),
    PreferExpressions.check,
    ProhibitAnnotations.check (ProhibitAnnotationsConfig (Just ["Override"])),
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

checkWithConfig :: [Config] -> (CompilationUnit -> FilePath -> [RDF.Diagnostic])
checkWithConfig config cUnit path = concatMap (\f -> f cUnit path) (checkRule config)

checkRule :: [Config] -> [CompilationUnit -> FilePath -> [RDF.Diagnostic]]
checkRule = map checkFromConfig

checkFromConfig :: Config -> (CompilationUnit -> FilePath -> [RDF.Diagnostic])
checkFromConfig (Config "AvoidMultipleTopLevelDecl" _ _) = AvoidMultipleTopLevelDecl.check
checkFromConfig (Config "AvoidMultipleVarDecl" _ _) = AvoidMultipleVarDecl.check
checkFromConfig (Config "AvoidNegations" _ _) = AvoidNegations.check
checkFromConfig (Config "AvoidStarImport" _ _) = AvoidStarImport.check
checkFromConfig (Config "CheckNonFinalMethodAttributes" _ _) = CheckNonFinalMethodAttributes.check
checkFromConfig (Config "CheckNonPrivateAttributes" _ _) = CheckNonPrivateAttributes.check
checkFromConfig (Config "ConsistentOverrideEqualsHashCode" _ _) = ConsistentOverrideEqualsHashCode.check
checkFromConfig (Config "DefaultComesLast" _ _) = DefaultComesLast.check
checkFromConfig (Config "InitializeVariables" _ _) = InitializeVariables.check
checkFromConfig (Config "NamingConventions" _ _) = NamingConventions.check
checkFromConfig (Config "NeedBraces" _ _) = NeedBraces.check
checkFromConfig (Config "NoLoopBreak" _ _) = NoLoopBreak.check
checkFromConfig (Config "NoNullPointerExceptionsForControl" _ _) = NoNullPointerExceptionsForControl.check
checkFromConfig (Config "ParameterNumber" mMax _) = ParameterNumber.check (ParameterNumberConfig mMax)
checkFromConfig (Config "PreferExpressions" _ _) = PreferExpressions.check
checkFromConfig (Config "ProhibitAnnotations" _ whitelist) = ProhibitAnnotations.check (ProhibitAnnotationsConfig whitelist)
checkFromConfig (Config "ReduceScope" _ _) = ReduceScope.check
checkFromConfig (Config "RedundantModifiers" _ _) = RedundantModifiers.check
checkFromConfig (Config "SameExecutionsInIf" _ _) = SameExecutionsInIf.check
checkFromConfig (Config "SimplifyBooleanReturn" _ _) = SimplifyBooleanReturn.check
checkFromConfig (Config "UseAssignOp" _ _) = UseAssignOp.check
checkFromConfig (Config "UseElse" _ _) = UseElse.check
checkFromConfig (Config "UseIncrementDecrementOperator" _ _) = UseIncrementDecrementOperator.check
checkFromConfig (Config "UseJavaArrayTypeStyle" _ _) = UseJavaArrayTypeStyle.check
checkFromConfig (Config "UsePostIncrementDecrement" _ _) = UsePostIncrementDecrement.check
checkFromConfig (Config {}) = \_ _ -> []
