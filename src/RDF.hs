{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RDF
  ( DiagnosticResult (..),
    Diagnostic (..),
    Location (..),
    Source (..),
    Severity (..),
    Position (..),
    Range (..),
    encodetojson,
    simpleDiagnostic,
    checkSeverityList,
    methodDiagnostic,
    rangeDiagnostic,
  )
where

import Data.Aeson
  ( ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
    pairs,
    (.=),
  )
import Data.ByteString.Lazy.Internal (ByteString)
import GHC.Generics (Generic)
import qualified Language.Java.Syntax as Java

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
    url :: Maybe String
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
    range :: Maybe Range
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
    resultSeverity :: Maybe Severity
  }
  deriving (Generic, Show)

data Severity
  = INFO
  | WARNING
  | ERROR
  deriving (Generic, Eq, Show, Ord)

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
  toEncoding (Diagnostic message location severity source code suggestions originalOutput) =
    pairs
      ( "message" .= message
          <> "location" .= location
          <> "severity" .= severity
          <> "source" .= source
          <> "code" .= code
          <> "suggestions" .= suggestions
          <> "original_output" .= originalOutput
      )

instance ToJSON DiagnosticResult where
  toEncoding (DiagnosticResult diagnostics resultSource maybeSeverity) =
    case maybeSeverity of
      Just resSeverity ->
        pairs
          ( "diagnostics" .= diagnostics
              <> "source" .= resultSource
              <> "severity" .= resSeverity
          )
      Nothing ->
        pairs
          ( "diagnostics" .= diagnostics
              <> "source" .= resultSource
          )

encodetojson :: ToJSON a => a -> Data.ByteString.Lazy.Internal.ByteString
encodetojson = encode

simpleDiagnostic :: String -> String -> Diagnostic
simpleDiagnostic dmessage fpath =
  Diagnostic
    { message = dmessage,
      location =
        Location
          { path = fpath,
            range = Nothing
          },
      severity = ERROR,
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }

checkSeverityList :: [Severity] -> Maybe Severity
checkSeverityList [] = Nothing
checkSeverityList list = Just (maximum list)

methodDiagnostic :: String -> String -> FilePath -> Diagnostic
methodDiagnostic methodName msg = simpleDiagnostic ("Method " ++ methodName ++ ": " ++ msg)

rangeFromSourceSpan :: Java.SourceSpan -> Range
rangeFromSourceSpan (start, end) =
  Range
    { start = Position {line = Java.loc_line start, column = Java.loc_column start},
      end = Just (Position {line = Java.loc_line end, column = Java.loc_column end})
    }

rangeDiagnostic :: String -> Java.SourceSpan -> FilePath -> Diagnostic
rangeDiagnostic msg range fPath =
  Diagnostic
    { message = msg,
      location =
        Location
          { path = fPath,
            range = Just (rangeFromSourceSpan range)
          },
      severity = ERROR,
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }
