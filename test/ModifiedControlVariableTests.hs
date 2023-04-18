module ModifiedControlVariableTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.ModifiedControlVariable (check)
import Language.Java.Syntax
import qualified RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/ModifiedControlVariable.java"
   in test [file ~: Tests.withParsedJavaFile file modifiedControlVariable]

modifiedControlVariable :: CompilationUnit -> FilePath -> Assertion
modifiedControlVariable cUnit path = do
  let expectedRanges =
        [ RDF.Range
            { RDF.start = RDF.Position {RDF.line = 10, RDF.column = Just 13},
              RDF.end = Just (RDF.Position {RDF.line = 10, RDF.column = Just 19})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 18, RDF.column = Just 13},
              RDF.end = Just (RDF.Position {RDF.line = 18, RDF.column = Just 22})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 22, RDF.column = Just 18},
              RDF.end = Just (RDF.Position {RDF.line = 22, RDF.column = Just 20})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 29, RDF.column = Just 14},
              RDF.end = Just (RDF.Position {RDF.line = 29, RDF.column = Just 16})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 31, RDF.column = Just 17},
              RDF.end = Just (RDF.Position {RDF.line = 31, RDF.column = Just 19})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 38, RDF.column = Just 14},
              RDF.end = Just (RDF.Position {RDF.line = 38, RDF.column = Just 16})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 39, RDF.column = Just 43},
              RDF.end = Just (RDF.Position {RDF.line = 39, RDF.column = Just 45})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 40, RDF.column = Just 17},
              RDF.end = Just (RDF.Position {RDF.line = 40, RDF.column = Just 19})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 41, RDF.column = Just 46},
              RDF.end = Just (RDF.Position {RDF.line = 41, RDF.column = Just 48})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 42, RDF.column = Just 21},
              RDF.end = Just (RDF.Position {RDF.line = 42, RDF.column = Just 23})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 51, RDF.column = Just 18},
              RDF.end = Just (RDF.Position {RDF.line = 51, RDF.column = Just 20})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 59, RDF.column = Just 18},
              RDF.end = Just (RDF.Position {RDF.line = 59, RDF.column = Just 20})
            }
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
