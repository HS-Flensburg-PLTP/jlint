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
    RDF.mkRange (22, 18) (22, 20),
    RDF.mkRange (29, 14) (29, 16),
    RDF.mkRange (31, 17) (31, 19),
    RDF.mkRange (38, 14) (38, 16),
    RDF.mkRange (39, 43) (39, 45),
    RDF.mkRange (40, 17) (40, 19),
    RDF.mkRange (41, 46) (41, 48),
    RDF.mkRange (42, 21) (42, 23),
    RDF.mkRange (51, 18) (51, 20),
    RDF.mkRange (59, 18) (59, 20)
  ]
