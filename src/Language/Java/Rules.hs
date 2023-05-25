module Language.Java.Rules where

import Config
import Config (ParameterNumberConfig (ParameterNumberConfig))
import Data.Map (Map, findWithDefault, fromList)
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

annotationswhitelist :: [String]
annotationswhitelist = ["Override"]

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
    ReduceScope.check,
    RedundantModifiers.check,
    UseAssignOp.check,
    UseElse.check,
    DeclarationOrder.check,
    UseIncrementDecrementOperator.check,
    ProhibitAnnotations.check annotationswhitelist,
    UseJavaArrayTypeStyle.check
  ]

checkAll :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks

checkWithConfig :: [Config] -> (CompilationUnit -> FilePath -> [RDF.Diagnostic])
checkWithConfig config cUnit path = concatMap (\f -> f cUnit path) (checkRule config)

checkRule :: [Config] -> [CompilationUnit -> FilePath -> [RDF.Diagnostic]]
checkRule = map foo

foo :: Config -> (CompilationUnit -> FilePath -> [RDF.Diagnostic])
foo (Config "AvoidMultipleTopLevelDecl" _) = AvoidMultipleTopLevelDecl.check
foo (Config "AvoidMultipleVarDecl" _) = AvoidMultipleVarDecl.check
foo (Config "AvoidNegations" _) = AvoidNegations.check
foo (Config "AvoidStarImport" _) = AvoidStarImport.check
foo (Config "CheckNonFinalMethodAttributes" _) = CheckNonFinalMethodAttributes.check
foo (Config "CheckNonPrivateAttributes" _) = CheckNonPrivateAttributes.check
foo (Config "ConsistentOverrideEqualsHashCode" _) = ConsistentOverrideEqualsHashCode.check
foo (Config "DefaultComesLast" _) = DefaultComesLast.check
foo (Config "InitializeVariables" _) = InitializeVariables.check
foo (Config "NamingConventions" _) = NamingConventions.check
foo (Config "NeedBraces" _) = NeedBraces.check
foo (Config "NoLoopBreak" _) = NoLoopBreak.check
foo (Config "NoNullPointerExceptionsForControl" _) = NoNullPointerExceptionsForControl.check
foo (Config "ParameterNumber" mMax) = ParameterNumber.check (ParameterNumberConfig mMax)
foo (Config "PreferExpressions" _) = PreferExpressions.check
foo (Config "ReduceScope" _) = ReduceScope.check
foo (Config "RedundantModifiers" _) = RedundantModifiers.check
foo (Config "SameExecutionsInIf" _) = SameExecutionsInIf.check
foo (Config "SimplifyBooleanReturn" _) = SimplifyBooleanReturn.check
foo (Config "UseAssignOp" _) = UseAssignOp.check
foo (Config "UseElse" _) = UseElse.check
foo (Config "UseIncrementDecrementOperator" _) = UseIncrementDecrementOperator.check
foo (Config "UseJavaArrayTypeStyle" _) = UseJavaArrayTypeStyle.check
foo (Config "UsePostIncrementDecrement" _) = UsePostIncrementDecrement.check
foo (Config _ _) = \_ _ -> []
