{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RDF (DiagnosticResult (..), Diagnostic (..), Location (..), encodetojson) where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding, toJSON), defaultOptions, encode, genericToEncoding, object)
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
  toJSON (Diagnostic message location severity source code suggestions originalOutput) = case severity of
    Nothing ->
      object
        [ "message" .= message,
          "location" .= location,
          "severity" .= severity,
          "source" .= source,
          "code" .= code,
          "suggestions" .= suggestions,
          "originalOutput" .= originalOutput
        ]
    Just either -> case either of
      Left str ->
        object
          [ "message" .= message,
            "location" .= location,
            "severity" .= str,
            "source" .= source,
            "code" .= code,
            "suggestions" .= suggestions,
            "originalOutput" .= originalOutput
          ]
      Right num ->
        object
          [ "message" .= message,
            "location" .= location,
            "severity" .= num,
            "source" .= source,
            "code" .= code,
            "suggestions" .= suggestions,
            "originalOutput" .= originalOutput
          ]

instance ToJSON DiagnosticResult where
  toJSON (DiagnosticResult diagnostics resultsource resultseverity) = case resultseverity of
    Nothing ->
      object
        [ "diagnostics" .= diagnostics,
          "resultSource" .= resultsource,
          "resultSeverity" .= resultseverity
        ]
    Just either -> case either of
      Left str ->
        object
          [ "diagnostics" .= diagnostics,
            "resultSource" .= resultsource,
            "resultSeverity" .= str
          ]
      Right num ->
        object
          [ "diagnostics" .= diagnostics,
            "resultSource" .= resultsource,
            "resultSeverity" .= num
          ]

encodetojson :: ToJSON a => a -> Data.ByteString.Lazy.Internal.ByteString
encodetojson = encode