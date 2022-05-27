{-# LANGUAGE DeriveGeneric #-}

module RDF (DiagnosticResult (..), Diagnostic (..), Location (..), encodetojson) where

import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON (toEncoding), genericToEncoding, defaultOptions, encode )

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Generic, Show, Eq)

data Range = Range
  { start :: Position,
    end :: Maybe Position
  }
  deriving (Generic, Show, Eq)

data Source = Source
  { name :: String,
    sourceURL :: Maybe String
  }
  deriving (Generic, Show, Eq)

data Code = Code
  { value :: String,
    codeURL :: Maybe String
  }
  deriving (Generic, Show, Eq)

data Suggestion = Suggestion
  { suggestionRange :: Range,
    text :: String
  }
  deriving (Generic, Show, Eq)

data Location = Location
  { path :: String,
    locationRange :: Maybe Range
  }
  deriving (Generic, Show, Eq)

data Diagnostic = Diagnostic
  { message :: String,
    location :: Location,
    severity :: Maybe (Either String Int),
    source :: Maybe Source,
    code :: Maybe Code,
    suggestions :: Maybe [Suggestion],
    originalOutput :: Maybe String
  }
  deriving (Generic, Show, Eq)

data DiagnosticResult = DiagnosticResult
  { diagnostics :: [Diagnostic],
    resultSource :: Maybe Source,
    resultSeverity :: Maybe (Either String Int)
  }
  deriving (Generic, Show)

instance ToJSON Position where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Range where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Source where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Code where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Suggestion where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Location where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Diagnostic where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DiagnosticResult where
  toEncoding = genericToEncoding defaultOptions

encodetojson datatype = 
      encode datatype