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
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            },
          RDF.Range
            { RDF.start = RDF.Position {RDF.line = 1, RDF.column = Just 1},
              RDF.end = Just (RDF.Position {RDF.line = 1, RDF.column = Just 1})
            }
        ]
  let diagnostic = check cUnit path
  assertEqual "Check number of messages" (length expectedRanges) (length diagnostic)
  zipWithM_
    (assertEqual "Check range")
    (map Just expectedRanges)
    (map (RDF.range . RDF.location) diagnostic)
