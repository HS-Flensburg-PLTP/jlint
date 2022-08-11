module NonPrivateAttributes.Tests (testAllNonPrivateAttributes) where

import CheckNonPrivateAttributes (check)
import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import RDF
import RessourceManager
import Test.HUnit

testAllNonPrivateAttributes :: IO ()
testAllNonPrivateAttributes = do
  testNonPrivateAttributes <- testNonPrivateAttributesIO

  runTestTT
    ( "Non Private Attributes" ~:
         [testNonPrivateAttributes]
    )
  return ()

testNonPrivateAttributesIO :: IO Test
testNonPrivateAttributesIO =
  do
    assertionList <-
      withCUnit
        "/test/NonPrivateAttributes/NonPrivateAttributes.java"
        ( return
            . map
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = CheckNonPrivateAttributes.check cUnit path
                  checkMessage diagResults "Method myname: Is not declared as private" path
                  checkPath diagResults path
              )
        )

    return ("Non Private Attributes" ~: test (map TestCase assertionList))
