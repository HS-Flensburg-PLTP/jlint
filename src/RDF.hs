{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RDF 
  ( DiagnosticResult (..), 
    Diagnostic (..), 
    Location (..), 
    Source (..), 
    Severity(..), 
    encodetojson, 
    simpleDiagnostic, 
    severityToList
  ) where

import Data.Aeson 
  ( ToJSON (toEncoding), 
    defaultOptions, 
    encode, 
    genericToEncoding
  )

import Data.ByteString.Lazy.Internal (ByteString)
import GHC.Generics (Generic)

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
    severity :: Severity,
    source :: Maybe Source,
    code :: Maybe Code,
    suggestions :: Maybe [Suggestion],
    originalOutput :: Maybe String
  }
  deriving (Generic, Show, Eq)

data DiagnosticResult = DiagnosticResult
  { diagnostics :: [Diagnostic],
    resultSource :: Maybe Source,
    resultSeverity :: Severity
  }
  deriving (Generic, Show)

data Severity = Unknown | Error | Warning | Info deriving (Generic, Eq)

instance ToJSON Severity where
  toEncoding = genericToEncoding defaultOptions

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

instance Show Severity where
  show Unknown = "Unkown"
  show Error = "Error"
  show Warning = "Warning"
  show Info = "Info"

instance Ord Severity where
  (<=) Info Info = True
  (<=) Info Warning = True
  (<=) Info Error = True
  (<=) Info Unknown = True
  (<=) Warning Warning = True
  (<=) Warning Error = True
  (<=) Warning Unknown = True
  (<=) Error Error = True
  (<=) Error Unknown = True
  (<=) Unknown Unknown = True
  (<=) Error Warning = False
  (<=) Error Info = False
  (<=) Warning Info = False
  (<=) Unknown _ = False
    
encodetojson :: ToJSON a => a -> Data.ByteString.Lazy.Internal.ByteString
encodetojson = encode

simpleDiagnostic :: String -> String -> Diagnostic
simpleDiagnostic message path =
  Diagnostic
    { message = message,
      location =
        Location
          { path = path,
            locationRange = Nothing
          },
      severity = Warning,
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }

severityToList :: [Diagnostic] -> [Severity]
severityToList [] = []
severityToList (Diagnostic _ _ s _ _ _ _ : xs) =
  s : severityToList xs