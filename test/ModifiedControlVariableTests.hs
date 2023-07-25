module ModifiedControlVariableTests where

import Control.Monad (zipWithM_)
import Language.Java.Rules.ModifiedControlVariable (check)
import qualified Language.Java.Rules.ModifiedControlVariable as ModifiedControlVariable
import Language.Java.Syntax
import qualified RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  Tests.rangesTest
    expectedRanges
    "ModifiedControlVariable.java"
    ModifiedControlVariable.check

expectedRanges :: [RDF.Range]
expectedRanges =
  [ RDF.mkRange (10, 13) (10, 19),
    RDF.mkRange (18, 13) (18, 22),
    RDF.mkRange (22, 17) (22, 20),
    RDF.mkRange (29, 13) (29, 16),
    RDF.mkRange (31, 17) (31, 20),
    RDF.mkRange (38, 13) (38, 16),
    RDF.mkRange (39, 42) (39, 45),
    RDF.mkRange (40, 17) (40, 20),
    RDF.mkRange (41, 46) (41, 49),
    RDF.mkRange (42, 21) (42, 24),
    RDF.mkRange (51, 17) (51, 20),
    RDF.mkRange (59, 17) (59, 20)
  ]
