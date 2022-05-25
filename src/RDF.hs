module RDF (DiagnosticResult, Diagnostic) where

data Position = Position
  { line :: Maybe Int,
    column :: Maybe Int
  }

data Range = Range
  { start :: Position,
    end :: Maybe Position
  }

data Source = Source
  { name :: String,
    url :: Maybe String
  }

data Code = Code
  { value :: String,
    url :: Maybe String
  }

data Suggestion = Suggestion
  { range :: Range,
    text :: String
  }

data Location = Location
  { path :: String,
    range :: Maybe Range
  }

data Diagnostic = Diagnostic
  { message :: String,
    location ::
      { path :: String,
        range :: Maybe Range
      },
    severity :: Maybe Either String Int,
    source :: Maybe Source,
    code :: Maybe Code,
    suggestions :: Maybe [Suggestion],
    original_output :: Maybe String
  }

data DiagnosticResult = DiagnosticResult
  { diagnostics :: [Diagnostic],
    source :: Maybe Source,
    severity :: Maybe Either String Int
  }
