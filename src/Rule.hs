{-# LANGUAGE OverloadedStrings #-}

module Rule (Rule (..)) where

import Control.Monad (unless)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types (Key, (.:!))
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty)
import Data.Yaml (FromJSON (..), Object, Parser, withObject, (.:))
import qualified Language.Java.Rules.AvoidNegations as AvoidNegations
import qualified Language.Java.Rules.AvoidOuterNegations as AvoidOuterNegations
import qualified Language.Java.Rules.AvoidStarImport as AvoidStarImport
import qualified Language.Java.Rules.ConsistentOverrideEqualsHashCode as ConsistentOverrideEqualsHashCode
import qualified Language.Java.Rules.DeclarationOrder as DeclarationOrder
import qualified Language.Java.Rules.DefaultComesLast as DefaultComesLast
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
import qualified Language.Java.Rules.NoCommonCodeInIf as NoCommonCodeInIf
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
import qualified Language.Java.Rules.SimplifyBoolean as SimplifyBoolean
import qualified Language.Java.Rules.SingleTopLevelClass as SingleTopLevelClass
import qualified Language.Java.Rules.SuppressWarnings as SuppressWarnings
import qualified Language.Java.Rules.UseAssignOp as UseAssignOp
import Language.Java.Rules.UseElse as UseElse (check)
import qualified Language.Java.Rules.UseIncrementDecrementOperator as UseIncrementDecrementOperator
import qualified Language.Java.Rules.UseJavaArrayTypeStyle as UseJavaArrayTypeStyle
import qualified Language.Java.Rules.UseLocalTypeInference as UseLocalTypeInference
import qualified Language.Java.Rules.UsePostIncrementDecrement as UsePostIncrementDecrement
import Language.Java.Syntax
import QualifiedIdent (QualifiedIdent)
import qualified RDF

newtype Rule = Rule {check :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]}

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \obj -> do
    rule <- obj .: "rule"
    case rule of
      "AvoidNegations" -> pure (Rule (liftIO AvoidNegations.check))
      "AvoidOuterNegations" -> pure (Rule (liftIO AvoidOuterNegations.check))
      "AvoidStarImport" -> pure (Rule (liftIO AvoidStarImport.check))
      "ConsistentOverrideEqualsHashCode" -> pure (Rule (liftIO ConsistentOverrideEqualsHashCode.check))
      "DeclarationOrder" -> pure (Rule (liftIO DeclarationOrder.check))
      "DefaultComesLast" -> pure (Rule (liftIO DefaultComesLast.check))
      "DuplicateInConditional" -> pure (Rule (liftIO NoCommonCodeInIf.check))
      "Evaluation" -> pure (Rule (liftIO Evaluation.check))
      "ExplicitValue" -> pure (Rule (liftIO ExplicitValue.check))
      "FinalParameters" -> pure (Rule (liftIO FinalParameters.check))
      "InitializeVariables" -> pure (Rule (liftIO InitializeVariables.check))
      "MethodInvocations" -> Rule . liftIO <$> parseMethodInvocations obj
      "MethodNames" -> Rule <$> parseMethodNames obj
      "ModifiedControlVariable" -> pure (Rule (liftIO ModifiedControlVariable.check))
      "MultipleStringLiterals" -> pure (Rule (liftIO MultipleStringLiterals.check))
      "MultipleVariableDeclarations" -> pure (Rule (liftIO MultipleVariableDeclarations.check))
      "NamingConventions" -> pure (Rule (liftIO NamingConventions.check))
      "NeedBraces" -> pure (Rule (liftIO NeedBraces.check))
      "NoAnnotations" -> Rule . liftIO <$> parseNoAnnotations obj
      "NoCasts" -> Rule . liftIO <$> parseNoCasts obj
      "NoCommonCodeInIf" -> pure (Rule (liftIO NoCommonCodeInIf.check))
      "NoDummyNames" -> pure (Rule (liftIO NoDummyNames.check))
      "NoExtraDataStructures" -> Rule . liftIO <$> parseNoExtraDataStructures obj
      "NoGermanNames" -> pure (Rule NoGermanNames.check)
      "NoIncDecInExpression" -> pure (Rule (liftIO NoIncDecInExpression.check))
      "NoLoopBreak" -> pure (Rule (liftIO NoLoopBreak.check))
      "NoNullPointerExceptionsForControl" -> pure (Rule (liftIO NoNullPointerExceptionsForControl.check))
      "ParameterNumber" -> Rule . liftIO <$> parseParameterNumber obj
      "Pattern" -> Rule . liftIO <$> parsePattern obj
      "PreferExpressions" -> pure (Rule (liftIO PreferExpressions.check))
      "PrivateAttributes" -> pure (Rule (liftIO PrivateAttributes.check))
      "ReduceScope" -> pure (Rule (liftIO ReduceScope.check))
      "RedundantLocalVariable" -> Rule . liftIO <$> parseRedundantLocalVariable obj
      "RedundantModifiers" -> pure (Rule (liftIO RedundantModifiers.check))
      "SimplifyBoolean" -> pure (Rule (liftIO SimplifyBoolean.check))
      "SingleTopLevelClass" -> pure (Rule (liftIO SingleTopLevelClass.check))
      "SuppressWarnings" -> Rule . liftIO <$> parseSuppressWarnings obj
      "UseAssignOp" -> pure (Rule (liftIO UseAssignOp.check))
      "UseElse" -> pure (Rule (liftIO UseElse.check))
      "UseIncrementDecrementOperator" -> pure (Rule (liftIO UseIncrementDecrementOperator.check))
      "UseJavaArrayTypeStyle" -> pure (Rule (liftIO UseJavaArrayTypeStyle.check))
      "UseLocalTypeInference" -> pure (Rule (liftIO UseLocalTypeInference.check))
      "UsePostIncrementDecrement" -> pure (Rule (liftIO UsePostIncrementDecrement.check))
      _ -> fail ("Unknown Rule: " ++ rule)

liftIO :: (a -> b -> c) -> a -> b -> IO c
liftIO f a b = pure (f a b)

-- Generic methods

parseMethodNameList :: Object -> Key -> Parser [QualifiedIdent]
parseMethodNameList obj key = do
  strings <- obj .: key
  checkNoExtraKeys obj [key]
  pure strings

checkNoExtraKeys :: Object -> [Key] -> Parser ()
checkNoExtraKeys obj allowedKeys = do
  let extraKeys = keys obj \\ ("rule" : allowedKeys)
  unless (null extraKeys) (fail ("Unexpected keys: " ++ show extraKeys))

-- Parsers for rules

parseMethodNames :: Object -> Parser (CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic])
parseMethodNames obj = MethodNames.check <$> parseStringList obj "whitelist"

parseNoExtraDataStructures :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parseNoExtraDataStructures obj =
  NoExtraDataStructures.check <$> parseNonEmptyStringList obj "methodNames"

parseNonEmptyStringList :: Object -> Key -> Parser (NonEmpty String)
parseNonEmptyStringList obj key = do
  strings <- obj .: key
  checkNoExtraKeys obj [key]
  pure strings

parseParameterNumber :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parseParameterNumber obj = do
  max <- obj .:! "max"
  checkNoExtraKeys obj ["max"]
  pure (ParameterNumber.check max)

parseNoAnnotations :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parseNoAnnotations obj = NoAnnotations.check <$> parseStringList obj "whitelist"

parseNoCasts :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parseNoCasts obj = NoCasts.check <$> parseMethodNameList obj "methodWhitelist"

parseSuppressWarnings :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parseSuppressWarnings obj =
  SuppressWarnings.check <$> parseMethodNameList obj "methodWhitelist"

parseStringList :: Object -> Key -> Parser [String]
parseStringList obj key = do
  strings <- obj .: key
  checkNoExtraKeys obj [key]
  pure strings

parseMethodInvocations :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parseMethodInvocations obj = do
  called <- obj .: "targetMethod"
  limited <- obj .: "limitedMethod"
  maxInv <- obj .: "maxInv"
  explanation <- obj .: "explanation"
  checkNoExtraKeys obj ["targetMethod", "limitedMethod", "maxInv", "explanation"]
  pure (MethodInvocations.check called limited maxInv explanation)

parseRedundantLocalVariable :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parseRedundantLocalVariable obj = do
  activated <- obj .:! "activated"
  checkNoExtraKeys obj ["activated"]
  pure (RedundantLocalVariable.check activated)

parsePattern :: Object -> Parser (CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic])
parsePattern obj = do
  pattern_ <- obj .: "pattern"
  explanation <- obj .: "explanation"
  checkNoExtraKeys obj ["pattern", "explanation"]
  pure (Pattern.check pattern_ explanation)
