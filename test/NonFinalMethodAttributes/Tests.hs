module NonFinalMethodAttributes.Tests (testAllNonFinalMethodAttributes) where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Rules.CheckNonFinalMethodAttributes (check)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAllNonFinalMethodAttributes :: IO ()
testAllNonFinalMethodAttributes = do
  testNonFinalMethodAttributes <- testNonFinalMethodAttributesIO

  runTestTT
    ( "Non Final Method Attributes" ~:
        [ testNonFinalMethodAttributes
        ]
    )
  return ()

testNonFinalMethodAttributesIO :: IO Test
testNonFinalMethodAttributesIO =
  do
    assertionList <-
      withCUnit
        "/test/NonFinalMethodAttributes/NonFinalMethodAttributes.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = check cUnit path
                  checkMessage diagResults "Method testFunc: age is not declared as Final" path
                  checkPath diagResults path
              )
        )

    return ("Non Final Method Attributes" ~: test (map TestCase assertionList))
