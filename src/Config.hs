{-# LANGUAGE OverloadedStrings #-}

module Config (Rule (..)) where

import Control.Monad (unless)
import Data.Aeson
  ( FromJSON (parseJSON),
    Key,
    Object,
    withObject,
    (.:),
    (.:!),
  )
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types (Parser)
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty)

data Rule
  = AvoidNegations
  | AvoidOuterNegations
  | AvoidStarImport
  | ConsistentOverrideEqualsHashCode
  | DeclarationOrder
  | DefaultComesLast
  | ExplicitValue
  | FinalParameters
  | InitializeVariables
  | MethodInvocations {called :: String, limited :: String, maxInv :: Int}
  | MethodNames {whitelist :: [String]}
  | ModifiedControlVariable
  | MultipleStringLiterals
  | MultipleVariableDeclarations
  | NamingConventions
  | NeedBraces
  | NoAnnotations {whitelist :: [String]}
  | NoCasts {whitelist :: [String]}
  | NoDummyNames
  | NoExtraDataStructures {methodNames :: NonEmpty String}
  | NoGermanNames
  | NoIncDecInExpression
  | NoLoopBreak
  | NoNullPointerExceptionsForControl
  | ParameterNumber {max :: Maybe Int}
  | PreferExpressions
  | PrivateAttributes
  | ReduceScope
  | RedundantModifiers
  | SameExecutionsInIf
  | SimplifyBoolean
  | SingleTopLevelClass
  | UseAssignOp
  | UseElse
  | UseIncrementDecrementOperator
  | UseJavaArrayTypeStyle
  | UsePostIncrementDecrement

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \obj -> do
    rule <- obj .: fromString "rule"
    case rule of
      "AvoidNegations" -> pure AvoidNegations
      "AvoidOuterNegations" -> pure AvoidOuterNegations
      "AvoidStarImport" -> pure AvoidStarImport
      "ConsistentOverrideEqualsHashCode" -> pure ConsistentOverrideEqualsHashCode
      "DeclarationOrder" -> pure DeclarationOrder
      "DefaultComesLast" -> pure DefaultComesLast
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
      "PreferExpressions" -> pure PreferExpressions
      "PrivateAttributes" -> pure PrivateAttributes
      "ReduceScope" -> pure ReduceScope
      "RedundantModifiers" -> pure RedundantModifiers
      "SameExecutionsInIf" -> pure SameExecutionsInIf
      "SimplifyBoolean" -> pure SimplifyBoolean
      "SingleTopLevelClass" -> pure SingleTopLevelClass
      "UseAssignOp" -> pure UseAssignOp
      "UseElse" -> pure UseElse
      "UseIncrementDecrementOperator" -> pure UseIncrementDecrementOperator
      "UseJavaArrayTypeStyle" -> pure UseJavaArrayTypeStyle
      "UsePostIncrementDecrement" -> pure UsePostIncrementDecrement
      _ -> fail ("Unknown Rule: " ++ rule)

parseParameterNumber :: Object -> Parser Rule
parseParameterNumber obj = do
  max <- obj .:! "max"
  checkNoExtraKeys obj ["max"]
  pure (ParameterNumber max)

parseMethodNames :: Object -> Parser Rule
parseMethodNames obj = MethodNames <$> parseStringList obj "whitelist"

parseNoAnnotations :: Object -> Parser Rule
parseNoAnnotations obj = NoAnnotations <$> parseStringList obj "whitelist"

parseNoCasts :: Object -> Parser Rule
parseNoCasts obj = NoCasts <$> parseStringList obj "whitelist"

parseStringList :: Object -> String -> Parser [String]
parseStringList obj key = do
  strings <- obj .: fromString key
  checkNoExtraKeys obj [fromString key]
  pure strings

parseNoExtraDataStructures :: Object -> Parser Rule
parseNoExtraDataStructures obj =
  NoExtraDataStructures <$> parseNonEmptyStringList obj "methodNames"

parseNonEmptyStringList :: Object -> Key -> Parser (NonEmpty String)
parseNonEmptyStringList obj key = do
  strings <- obj .: key
  checkNoExtraKeys obj [key]
  pure strings

parseMethodInvocations :: Object -> Parser Rule
parseMethodInvocations obj = do
  called <- obj .: "called"
  limited <- obj .: "limited"
  maxInv <- obj .: "maxInv"
  checkNoExtraKeys obj ["called", "limited", "maxInv"]
  pure (MethodInvocations called limited maxInv)

checkNoExtraKeys :: Object -> [Key] -> Parser ()
checkNoExtraKeys obj allowedKeys = do
  let extraKeys = keys obj \\ ("rule" : allowedKeys)
  unless (null extraKeys) (fail ("Unexpected keys: " ++ show extraKeys))
