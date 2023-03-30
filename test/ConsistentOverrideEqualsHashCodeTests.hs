module ConsistentOverrideEqualsHashCodeTests where

import Language.Java.Syntax (CompilationUnit)
import RDF
import Test.HUnit
import qualified Tests

tests :: Test
tests =
  let file = "/test/java/ConsistentOverrideEqualsHashCode.java"
   in test [file ~: Tests.withParsedJavaFile file consistentOverride]

consistentOverride :: CompilationUnit -> FilePath -> Assertion
consistentOverride cUnit path = do