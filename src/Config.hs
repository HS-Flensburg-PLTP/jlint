{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
import GHC.Generics (Generic)

data Config = Config
  {rule :: String, max :: Maybe Int, whitelist :: Maybe [String]}
  deriving (Show, Generic)

instance FromJSON Config

newtype ParameterNumberConfig = ParameterNumberConfig {maxParameterNumber :: Maybe Int}

newtype ProhibitAnnotationsConfig = ProhibitAnnotationsConfig {annotationWhitelist :: Maybe [String]}
