{-# LANGUAGE BlockArguments #-}

module Config where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types
import Data.List ((\\))

data Rule
  = AvoidMultipleTopLevelDecl
  | AvoidMultipleVarDecl
  | AvoidNegations
  | AvoidStarImport
  | CheckNonFinalMethodAttributes
  | CheckNonPrivateAttributes
  | ConsistentOverrideEqualsHashCode
  | DeclarationOrder
  | DefaultComesLast
  | InitializeVariables
  | ModifiedControlVariable
  | NamingConventions
  | NeedBraces
  | NoLoopBreak
  | NoNullPointerExceptionsForControl
  | ParameterNumber {max :: Maybe Int, min :: Maybe Int}
  | PreferExpressions
  | ProhibitAnnotations {whitelist :: [String]}
  | ReduceScope
  | RedundantModifiers
  | SameExecutionsInIf
  | SimplifyBooleanReturn
  | UnusedLocalVariable
  | UseAssignOp
  | UseElse
  | UseIncrementDecrementOperator
  | UseJavaArrayTypeStyle
  | UsePostIncrementDecrement

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \obj -> do
    rule <- obj .: fromString "rule"
    case rule of
      "AvoidMultipleTopLevelDecl" -> pure AvoidMultipleTopLevelDecl
      "AvoidMultipleVarDecl" -> pure AvoidMultipleVarDecl
      "AvoidNegations" -> pure AvoidNegations
      "AvoidStarImport" -> pure AvoidStarImport
      "CheckNonFinalMethodAttributes" -> pure CheckNonFinalMethodAttributes
      "CheckNonPrivateAttributes" -> pure CheckNonPrivateAttributes
      "ConsistentOverrideEqualsHashCode" -> pure ConsistentOverrideEqualsHashCode
      "DeclarationOrder" -> pure DeclarationOrder
      "DefaultComesLast" -> pure DefaultComesLast
      "InitializeVariables" -> pure InitializeVariables
      "ModifiedControlVariable" -> pure ModifiedControlVariable
      "NamingConventions" -> pure NamingConventions
      "NeedBraces" -> pure NeedBraces
      "NoLoopBreak" -> pure NoLoopBreak
      "NoNullPointerExceptionsForControl" -> pure NoNullPointerExceptionsForControl
      "ParameterNumber" -> parseParameterNumber obj
      "PreferExpressions" -> pure PreferExpressions
      "ProhibitAnnotations" -> parseProhibitAnnotations obj
      "ReduceScope" -> pure ReduceScope
      "RedundantModifiers" -> pure RedundantModifiers
      "SameExecutionsInIf" -> pure SameExecutionsInIf
      "SimplifyBooleanReturn" -> pure SimplifyBooleanReturn
      "UnusedLocalVariable" -> pure UnusedLocalVariable
      "UseAssignOp" -> pure UseAssignOp
      "UseElse" -> pure UseElse
      "UseIncrementDecrementOperator" -> pure UseIncrementDecrementOperator
      "UseJavaArrayTypeStyle" -> pure UseJavaArrayTypeStyle
      "UsePostIncrementDecrement" -> pure UsePostIncrementDecrement
      _ -> fail ("Unknown Rule: " ++ rule)

parseParameterNumber :: Object -> Parser Rule
parseParameterNumber obj = do
  hasMax <- obj .:? fromString "max"
  hasMin <- obj .:? fromString "min"
  case (hasMax, hasMin) of
    (Just maxVal, Nothing) -> do
      checkNoExtraKeys obj [fromString "max", fromString "min"]
      pure (ParameterNumber maxVal Nothing)
    (Nothing, Just minVal) -> do
      checkNoExtraKeys obj [fromString "max", fromString "min"]
      pure (ParameterNumber Nothing minVal)
    (Just maxVal, Just minVal) -> do
      checkNoExtraKeys obj [fromString "max", fromString "min"]
      pure (ParameterNumber maxVal minVal)
    (Nothing, Nothing) -> fail "Required field 'max' or 'min' is missing"

parseProhibitAnnotations :: Object -> Parser Rule
parseProhibitAnnotations obj = do
  hasWhitelist <- obj .:? fromString "whitelist"
  case hasWhitelist of
    Just whitelistVal -> do
      checkNoExtraKeys obj [fromString "whitelist"]
      pure (ProhibitAnnotations whitelistVal)
    Nothing -> fail "Required field 'whitelist' is missing"

checkNoExtraKeys :: Object -> [Key] -> Parser ()
checkNoExtraKeys obj allowedKeys = do
  let objKeys = keys obj
  let extraKeys = objKeys \\ (fromString "rule" : allowedKeys)
  unless (null extraKeys) $
    fail $
      "Unexpected keys: " ++ show extraKeys
