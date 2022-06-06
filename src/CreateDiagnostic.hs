module CreateDiagnostic where

import RDF (Diagnostic(..), Location (..))

constructDiagnostic :: String -> String -> Diagnostic
constructDiagnostic varname message=
  Diagnostic
    { message = message,
      location =
        Location
          { path = "test/Strings2.java",
            locationRange = Nothing
          },
      severity = Just (Left "WARNING"),
      source = Nothing,
      code = Nothing,
      suggestions = Nothing,
      originalOutput = Nothing
    }