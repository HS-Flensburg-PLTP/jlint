{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
import GHC.Generics

newtype Config = Config
  { rules :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Config
