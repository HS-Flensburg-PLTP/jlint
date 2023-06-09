{-# LANGUAGE BlockArguments #-}

module Config where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types
import Data.List ((\\))

data Rule
  = ParameterNumber {max :: Maybe Int, min :: Maybe Int}
  | ProhibitAnnotations {whitelist :: [String]}

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \obj -> do
    rule <- obj .: fromString "rule"
    case rule of
      "ParameterNumber" -> parseParameterNumber obj
      "ProhibitAnnotations" -> parseProhibitAnnotations obj
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
