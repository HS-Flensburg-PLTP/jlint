{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
import GHC.Generics (Generic)

newtype Config = Config
  {rule :: String}
  deriving (Show, Generic)

instance FromJSON Config
