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
  | NoCasts {whitelist :: [String]}
  | NoLoopBreak
  | NoNullPointerExceptionsForControl
  | ParameterNumber {max :: Maybe Int}
  | PreferExpressions
  | ProhibitAnnotations {whitelist :: [String]}
  | ProhibitMyIdentPrefix
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
      "NoCasts" -> parseNoCasts obj
      "NoLoopBreak" -> pure NoLoopBreak
      "NoNullPointerExceptionsForControl" -> pure NoNullPointerExceptionsForControl
      "ParameterNumber" -> parseParameterNumber obj
      "PreferExpressions" -> pure PreferExpressions
      "ProhibitAnnotations" -> parseProhibitAnnotations obj
      "ProhibitMyIdentPrefix" -> pure ProhibitMyIdentPrefix
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
  max <- obj .:! fromString "max"
  checkNoExtraKeys obj [fromString "max"]
  pure (ParameterNumber max)

parseProhibitAnnotations :: Object -> Parser Rule
parseProhibitAnnotations obj = ProhibitAnnotations <$> parseWhitelist obj

parseNoCasts :: Object -> Parser Rule
parseNoCasts obj = NoCasts <$> parseWhitelist obj

parseWhitelist :: Object -> Parser [String]
parseWhitelist obj = do
  whitelistVal <- obj .: fromString "whitelist"
  checkNoExtraKeys obj [fromString "whitelist"]
  pure whitelistVal

checkNoExtraKeys :: Object -> [Key] -> Parser ()
checkNoExtraKeys obj allowedKeys = do
  let objKeys = keys obj
  let extraKeys = objKeys \\ (fromString "rule" : allowedKeys)
  unless (null extraKeys) $
    fail $
      "Unexpected keys: " ++ show extraKeys
