import CheckScope.Tests
import DefaultComesLast.Tests
import EmptyLoopBody.Tests
import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import qualified InitializeVariablesTests
import NamingConventions.Tests
import NeedBraces.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import qualified NoNullPointerExceptionsForControlTests
import NonFinalMethodAttributes.Tests (testAllNonFinalMethodAttributes)
import NonPrivateAttributes.Tests (testAllNonPrivateAttributes)
import qualified PreferExpressionsTests
import qualified ReduceScopeTests
import SameExecutionsInIf.Tests
import SimplifyBooleanReturn.Tests
import Test.HUnit (runTestTT)
import UnnecessaryVariables.Tests (testAllUnnecessaryVariables)
import qualified UseElseTests

main :: IO ()
main = do
  testAllEmptyLoopBodies
  CheckScope.Tests.testAll
  testAllUnnecessaryVariables
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  testAllNonFinalMethodAttributes
  testAllNonPrivateAttributes
  -- SameExecutionsInIf.Tests.testAll
  DefaultComesLast.Tests.testAll
  SimplifyBooleanReturn.Tests.testAll
  NeedBraces.Tests.testAll
  runTestTT InitializeVariablesTests.tests
  runTestTT NoNullPointerExceptionsForControlTests.tests
  runTestTT PreferExpressionsTests.tests
  runTestTT ReduceScopeTests.tests
  runTestTT UseElseTests.tests
  return ()
