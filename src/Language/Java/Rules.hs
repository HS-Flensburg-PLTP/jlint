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
checkRule = map foo

foo :: Config -> (CompilationUnit -> FilePath -> [RDF.Diagnostic])
foo (Config "AvoidMultipleTopLevelDecl" _ _) = AvoidMultipleTopLevelDecl.check
foo (Config "AvoidMultipleVarDecl" _ _) = AvoidMultipleVarDecl.check
foo (Config "AvoidNegations" _ _) = AvoidNegations.check
foo (Config "AvoidStarImport" _ _) = AvoidStarImport.check
foo (Config "CheckNonFinalMethodAttributes" _ _) = CheckNonFinalMethodAttributes.check
foo (Config "CheckNonPrivateAttributes" _ _) = CheckNonPrivateAttributes.check
foo (Config "ConsistentOverrideEqualsHashCode" _ _) = ConsistentOverrideEqualsHashCode.check
foo (Config "DefaultComesLast" _ _) = DefaultComesLast.check
foo (Config "InitializeVariables" _ _) = InitializeVariables.check
foo (Config "NamingConventions" _ _) = NamingConventions.check
foo (Config "NeedBraces" _ _) = NeedBraces.check
foo (Config "NoLoopBreak" _ _) = NoLoopBreak.check
foo (Config "NoNullPointerExceptionsForControl" _ _) = NoNullPointerExceptionsForControl.check
foo (Config "ParameterNumber" mMax _) = ParameterNumber.check (ParameterNumberConfig mMax)
foo (Config "PreferExpressions" _ _) = PreferExpressions.check
foo (Config "ProhibitAnnotations" _ whitelist) = ProhibitAnnotations.check (ProhibitAnnotationsConfig whitelist)
foo (Config "ReduceScope" _ _) = ReduceScope.check
foo (Config "RedundantModifiers" _ _) = RedundantModifiers.check
foo (Config "SameExecutionsInIf" _ _) = SameExecutionsInIf.check
foo (Config "SimplifyBooleanReturn" _ _) = SimplifyBooleanReturn.check
foo (Config "UseAssignOp" _ _) = UseAssignOp.check
foo (Config "UseElse" _ _) = UseElse.check
foo (Config "UseIncrementDecrementOperator" _ _) = UseIncrementDecrementOperator.check
foo (Config "UseJavaArrayTypeStyle" _ _) = UseJavaArrayTypeStyle.check
foo (Config "UsePostIncrementDecrement" _ _) = UsePostIncrementDecrement.check
foo (Config {}) = \_ _ -> []
