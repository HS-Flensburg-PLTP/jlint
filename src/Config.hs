{-# LANGUAGE OverloadedStrings #-}

module Config (Rule (..)) where

import Control.Monad (unless)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types (Key, (.:!))
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty)
import Data.Yaml (FromJSON (..), Object, Parser, withObject, (.:))
import Language.Java.Rules.Pattern (Pattern)
import QualifiedIdent (QualifiedIdent)

data Rule
  = AvoidNegations
  | AvoidOuterNegations
  | AvoidStarImport
  | ConsistentOverrideEqualsHashCode
  | DeclarationOrder
  | DefaultComesLast
  | DuplicateInConditional
  | Evaluation
  | ExplicitValue
  | FinalParameters
  | InitializeVariables
  | MethodInvocations {called :: QualifiedIdent, limited :: String, maxInv :: Int, explanation :: String}
  | MethodNames {whitelist :: [String]}
  | ModifiedControlVariable
  | MultipleStringLiterals
  | MultipleVariableDeclarations
  | NamingConventions
  | NeedBraces
  | NoAnnotations {whitelist :: [String]}
  | NoCasts {methodWhitelist :: [QualifiedIdent]}
  | NoDummyNames
  | NoExtraDataStructures {methodNames :: NonEmpty String}
  | NoGermanNames
  | NoIncDecInExpression
  | NoLoopBreak
  | NoNullPointerExceptionsForControl
  | ParameterNumber {max :: Maybe Int}
  | Pattern {pattern_ :: Pattern, explanation :: String}
  | PreferExpressions
  | PrivateAttributes
  | ReduceScope
  | RedundantLocalVariable {activated :: Maybe Bool}
  | RedundantModifiers
  | SameExecutionsInIf
  | Simplify
  | SimplifyBoolean
  | SingleTopLevelClass
  | SuppressWarnings {methodWhitelist :: [QualifiedIdent]}
  | UseAssignOp
  | UseElse
  | UseIncrementDecrementOperator
  | UseJavaArrayTypeStyle
  | UsePostIncrementDecrement

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \obj -> do
    rule <- obj .: "rule"
    case rule of
      "AvoidNegations" -> pure AvoidNegations
      "AvoidOuterNegations" -> pure AvoidOuterNegations
      "AvoidStarImport" -> pure AvoidStarImport
      "ConsistentOverrideEqualsHashCode" -> pure ConsistentOverrideEqualsHashCode
      "DeclarationOrder" -> pure DeclarationOrder
      "DefaultComesLast" -> pure DefaultComesLast
      "DuplicateInConditional" -> pure DuplicateInConditional
      "Evaluation" -> pure Evaluation
      "ExplicitValue" -> pure ExplicitValue
      "FinalParameters" -> pure FinalParameters
      "InitializeVariables" -> pure InitializeVariables
      "MethodInvocations" -> parseMethodInvocations obj
      "MethodNames" -> parseMethodNames obj
      "ModifiedControlVariable" -> pure ModifiedControlVariable
      "MultipleStringLiterals" -> pure MultipleStringLiterals
      "MultipleVariableDeclarations" -> pure MultipleVariableDeclarations
      "NamingConventions" -> pure NamingConventions
      "NeedBraces" -> pure NeedBraces
      "NoAnnotations" -> parseNoAnnotations obj
      "NoCasts" -> parseNoCasts obj
      "NoDummyNames" -> pure NoDummyNames
      "NoExtraDataStructures" -> parseNoExtraDataStructures obj
      "NoGermanNames" -> pure NoGermanNames
      "NoIncDecInExpression" -> pure NoIncDecInExpression
      "NoLoopBreak" -> pure NoLoopBreak
      "NoNullPointerExceptionsForControl" -> pure NoNullPointerExceptionsForControl
      "ParameterNumber" -> parseParameterNumber obj
      "Pattern" -> parsePattern obj
      "PreferExpressions" -> pure PreferExpressions
      "PrivateAttributes" -> pure PrivateAttributes
      "ReduceScope" -> pure ReduceScope
      "RedundantLocalVariable" -> parseRedundantLocalVariable obj
      "RedundantModifiers" -> pure RedundantModifiers
      "SameExecutionsInIf" -> pure SameExecutionsInIf
      "Simplify" -> pure Simplify
      "SimplifyBoolean" -> pure SimplifyBoolean
      "SingleTopLevelClass" -> pure SingleTopLevelClass
      "SuppressWarnings" -> parseSuppressWarnings obj
      "UseAssignOp" -> pure UseAssignOp
      "UseElse" -> pure UseElse
      "UseIncrementDecrementOperator" -> pure UseIncrementDecrementOperator
      "UseJavaArrayTypeStyle" -> pure UseJavaArrayTypeStyle
      "UsePostIncrementDecrement" -> pure UsePostIncrementDecrement
      _ -> fail ("Unknown Rule: " ++ rule)

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

parseMethodNames :: Object -> Parser Rule
parseMethodNames obj = MethodNames <$> parseStringList obj "whitelist"

parseNoExtraDataStructures :: Object -> Parser Rule
parseNoExtraDataStructures obj =
  NoExtraDataStructures <$> parseNonEmptyStringList obj "methodNames"

parseNonEmptyStringList :: Object -> Key -> Parser (NonEmpty String)
parseNonEmptyStringList obj key = do
  strings <- obj .: key
  checkNoExtraKeys obj [key]
  pure strings

parseParameterNumber :: Object -> Parser Rule
parseParameterNumber obj = do
  max <- obj .:! "max"
  checkNoExtraKeys obj ["max"]
  pure (ParameterNumber max)

parseNoAnnotations :: Object -> Parser Rule
parseNoAnnotations obj = NoAnnotations <$> parseStringList obj "whitelist"

parseNoCasts :: Object -> Parser Rule
parseNoCasts obj = NoCasts <$> parseMethodNameList obj "methodWhitelist"

parseSuppressWarnings :: Object -> Parser Rule
parseSuppressWarnings obj =
  SuppressWarnings <$> parseMethodNameList obj "methodWhitelist"

parseStringList :: Object -> Key -> Parser [String]
parseStringList obj key = do
  strings <- obj .: key
  checkNoExtraKeys obj [key]
  pure strings

parseMethodInvocations :: Object -> Parser Rule
parseMethodInvocations obj = do
  called <- obj .: "targetMethod"
  limited <- obj .: "limitedMethod"
  maxInv <- obj .: "maxInv"
  explanation <- obj .: "explanation"
  checkNoExtraKeys obj ["targetMethod", "limitedMethod", "maxInv", "explanation"]
  pure (MethodInvocations called limited maxInv explanation)

parseRedundantLocalVariable :: Object -> Parser Rule
parseRedundantLocalVariable obj = do
  activated <- obj .:! "activated"
  checkNoExtraKeys obj ["activated"]
  pure (RedundantLocalVariable activated)

parsePattern :: Object -> Parser Rule
parsePattern obj = do
  pattern_ <- obj .: "pattern"
  explanation <- obj .: "explanation"
  checkNoExtraKeys obj ["pattern", "explanation"]
  pure (Pattern pattern_ explanation)
