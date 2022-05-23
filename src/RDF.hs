module RDF where

data Position = Position
  { line :: Int,
    column :: Int
  }

data Range = Range
  { start :: Position,
    end :: Position
  }

data Source = Source
  { name :: String,
    url :: String
  }

data Code = Code
  { value :: String,
    url :: String
  }

data Suggestion = Suggestion
  { range :: Range,
    text :: String
  }

data Location = Location
  { path :: String,
    range :: Range
  }

data Diagnostic = Diagnostic
  { message :: String,
    location ::
      { path :: String,
        range :: Range
      },
    severity :: oneOf String / Int,
    source :: Source,
    code :: Code,
    suggestions :: [Suggestion],
    original_output :: String
  }

data DiagnosticResult = DiagnosticResult
  { diagnostics :: [Diagnostic],
    source :: Source,
    severity :: oneOf String / Int
  }
