module RDF (DiagnosticResult (..), Diagnostic (..), Location (..)) where

data URL = Maybe String

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq)

data Range = Range
  { start :: Position,
    end :: Maybe Position
  }
  deriving (Show, Eq)

data Source = Source
  { name :: String,
    sourceURL :: Maybe String
  }
  deriving (Show, Eq)

data Code = Code
  { value :: String,
    codeURL :: Maybe String
  }
  deriving (Show, Eq)

data Suggestion = Suggestion
  { suggestionRange :: Range,
    text :: String
  }
  deriving (Show, Eq)

data Location = Location
  { path :: String,
    locationRange :: Maybe Range
  }
  deriving (Show, Eq)

data Diagnostic = Diagnostic
  { message :: String,
    location :: Location,
    severity :: Maybe (Either String Int),
    source :: Maybe Source,
    code :: Maybe Code,
    suggestions :: Maybe [Suggestion],
    originalOutput :: Maybe String
  }
  deriving (Show, Eq)

data DiagnosticResult = DiagnosticResult
  { diagnostics :: [Diagnostic],
    resultSource :: Maybe Source,
    resultSeverity :: Maybe (Either String Int)
  }
  deriving (Show)
