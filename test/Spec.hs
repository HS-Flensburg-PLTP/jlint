import qualified AvoidMultipleVarDeclTests
import qualified AvoidNegationsTests
import qualified AvoidStarImportTests
import qualified ConsistentOverrideEqualsHashCodeTests
import DefaultComesLast.Tests
import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import qualified InitializeVariablesTests
import NamingConventions.Tests
import NeedBraces.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import qualified NoNullPointerExceptionsForControlTests
import NonFinalMethodAttributes.Tests (testAllNonFinalMethodAttributes)
import NonPrivateAttributes.Tests (testAllNonPrivateAttributes)
import qualified ParameterNumberTests
import qualified PreferExpressionsTests
import qualified ReduceScopeTests
import qualified RedundantModifiersTests
import SameExecutionsInIf.Tests
import SimplifyBooleanReturn.Tests
import Test.HUnit (Test, runTestTT, test, (~:))
import qualified UseAssignOpTests
import qualified UseElseTests
import qualified UseIncrementDecrementOperatorTests
import qualified UseJavaArrayTypeStyleTests

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  testAllNonFinalMethodAttributes
  testAllNonPrivateAttributes
  -- SameExecutionsInIf.Tests.testAll
  DefaultComesLast.Tests.testAll
  SimplifyBooleanReturn.Tests.testAll
  NeedBraces.Tests.testAll
  runTestTT tests
  return ()

tests :: Test
tests =
  test
    [ "AvoidMultipleVarDecl" ~: AvoidMultipleVarDeclTests.tests,
      "AvoidNegations" ~: AvoidNegationsTests.tests,
      "AvoidStarImport" ~: AvoidStarImportTests.tests,
      "InitialzeVariables" ~: InitializeVariablesTests.tests,
      "NoNullPointerExpceptionsForControl" ~: NoNullPointerExceptionsForControlTests.tests,
      "ParameterNumber" ~: ParameterNumberTests.tests,
      "PreferExpressions" ~: PreferExpressionsTests.tests,
      "ReduceScope" ~: ReduceScopeTests.tests,
      "RedundantModifiers" ~: RedundantModifiersTests.tests,
      "UseAssignOp" ~: UseAssignOpTests.tests,
      "UseElse" ~: UseElseTests.tests,
      "UseIncrementDecrementOperator" ~: UseIncrementDecrementOperatorTests.tests,
      "UseJavaArrayTypeStyle" ~: UseJavaArrayTypeStyleTests.tests,
      "ConsistentOverrideEqualsHashCode" ~: ConsistentOverrideEqualsHashCodeTests.tests
    ]
