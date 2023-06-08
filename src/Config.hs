module Config where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types
import Data.List ((\\))

data Rule
  = ParameterNumber {max :: Int}
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
  case hasMax of
    Just max -> do
      checkNoExtraKeys obj [fromString "max"]
      pure (ParameterNumber max)
    Nothing -> fail "Required field 'max' is missing"

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
