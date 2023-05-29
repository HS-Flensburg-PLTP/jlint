module Language.Java.Rules where

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
import qualified Language.Java.Rules.ProhibitGermanNames as ProhibitGermanNames
import qualified Language.Java.Rules.ReduceScope as ReduceScope
import qualified Language.Java.Rules.RedundantModifiers as RedundantModifiers
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import qualified Language.Java.Rules.UseIncrementDecrementOperator as UseIncrementDecrementOperator
import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import Language.Java.Syntax
import qualified RDF

annotationswhitelist :: [String]
annotationswhitelist = ["Override"]

checks :: [CompilationUnit -> FilePath -> [RDF.Diagnostic]]
checks =
  []

{-AvoidMultipleTopLevelDecl.check,
AvoidMultipleVarDecl.check,
AvoidNegations.check,
AvoidStarImport.check,
ConsistentOverrideEqualsHashCode.check,
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
UseJavaArrayTypeStyle.check,
ProhibitMyIdentPrefix.check, -}

checksIO :: [CompilationUnit -> FilePath -> IO [RDF.Diagnostic]]
checksIO =
  [ ProhibitGermanNames.check
  ]

checkAll :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks

checkAllIO :: CompilationUnit -> FilePath -> IO [RDF.Diagnostic]
checkAllIO = executeAll checksIO

executeAll :: [CompilationUnit -> FilePath -> IO [RDF.Diagnostic]] -> CompilationUnit -> FilePath -> IO [RDF.Diagnostic]
executeAll [] _ _ = return []
executeAll (check : checks) cUnit path = do
  result <- check cUnit path
  results <- executeAll checks cUnit path
  return (result ++ results)
