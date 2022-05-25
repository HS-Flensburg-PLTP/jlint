module RDF (DiagnosticResult, Diagnostic) where

data URL = Maybe String

data Position = Position
  { line :: Int,
    column :: Int
  }

data Range = Range
  { start :: Position,
    end :: Maybe Position
  }

data Source = Source
  { name :: String,
    sourceUrl :: Maybe String
  }

data Code = Code
  { value :: String,
    codeUrl :: Maybe String
  }

data Suggestion = Suggestion
  { suggestionRange :: Range,
    text :: String
  }

data Location = Location
  { path :: String,
    locationRange :: Maybe Range
  }

data Diagnostic = Diagnostic
  { message :: String,
    location :: Location,
    severity :: Maybe (Either String Int),
    source :: Maybe Source,
    code :: Maybe Code,
    suggestions :: Maybe [Suggestion],
    originalOutput :: Maybe String
  }

data DiagnosticResult = DiagnosticResult
  { diagnostics :: [Diagnostic],
    resultSource :: Maybe Source,
    resultSeverity :: Maybe (Either String Int)
  }
