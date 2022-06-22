{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RDF (DiagnosticResult (..), Diagnostic (..), Location (..), Source (..), encodetojson, simpleDiagnostic, methodDiagnostic) where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding, toJSON), ToJSON1 (liftToJSON), defaultOptions, encode, genericToEncoding, object)
import Data.Aeson.Types
  ( Value,
    listValue,
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
  toJSON (Diagnostic message location severity source code suggestions originalOutput) =
    object
      [ "message" .= message,
        "location" .= location,
        "severity" .= liftToJSON eitherToJSON (listValue eitherToJSON) severity,
        "source" .= source,
        "code" .= code,
        "suggestions" .= suggestions,
        "originalOutput" .= originalOutput
      ]

instance ToJSON DiagnosticResult where
  toJSON (DiagnosticResult diagnostics resultsource resultseverity) =
    object
      [ "diagnostics" .= diagnostics,
        "resultSource" .= resultsource,
        "resultSeverity" .= liftToJSON eitherToJSON (listValue eitherToJSON) resultseverity
      ]

encodetojson :: ToJSON a => a -> Data.ByteString.Lazy.Internal.ByteString
encodetojson = encode

eitherToJSON :: (ToJSON v1, ToJSON v2) => Either v1 v2 -> Value
eitherToJSON (Left v1) = toJSON v1
eitherToJSON (Right v2) = toJSON v2

simpleDiagnostic :: String -> FilePath -> Diagnostic
simpleDiagnostic message path =
  Diagnostic
    { message = message,
      location =
        Location
          { path = path,
            locationRange = Nothing
          },
      severity = Just (Left "WARNING"),
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }

methodDiagnostic :: String -> String -> FilePath -> Diagnostic
methodDiagnostic methodName msg path =
  simpleDiagnostic ("Method " ++ methodName ++ ": " ++ msg) path
