module NoAnnotationsTests where

import Control.Monad.Identity (zipWithM_)
import Language.Java.Rules.NoAnnotations as NoAnnotations (check)
import Language.Java.Syntax (CompilationUnit)
import qualified RDF
import Test.HUnit
import Tests
import qualified Tests

tests :: Test
tests =
  rangesTest
    expectedRanges
    "NoAnnotations.java"
    (NoAnnotations.check ["FooBar", "Override"])

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (41, 5) (41, 12),
    RDF.mkRange (43, 5) (43, 25),
    RDF.mkRange (45, 5) (45, 19),
    RDF.mkRange (46, 5) (46, 22),
    RDF.mkRange (47, 5) (47, 35),
    RDF.mkRange (48, 5) (48, 43)
  ]
