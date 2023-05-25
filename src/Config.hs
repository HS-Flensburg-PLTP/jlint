{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
import GHC.Generics (Generic)

data Config = Config
  {rule :: String, max :: Maybe Int}
  deriving (Show, Generic)

instance FromJSON Config

newtype ParameterNumberConfig = ParameterNumberConfig {maxParameterNumber :: Maybe Int}
