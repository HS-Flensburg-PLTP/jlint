import CheckScope.Tests
import DefaultComesLast.Tests
import EmptyLoopBody.Tests
import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NeedBraces.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import NonFinalMethodAttributes.Tests (testAllNonFinalMethodAttributes)
import NonPrivateAttributes.Tests (testAllNonPrivateAttributes)
import SameExecutionsInIf.Tests
import SimplifyBooleanReturn.Tests
import UnnecessaryVariables.Tests (testAllUnnecessaryVariables)

main :: IO ()
main = do
  testAllEmptyLoopBodies
  CheckScope.Tests.testAll
  testAllUnnecessaryVariables
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  testAllNonFinalMethodAttributes
  testAllNonPrivateAttributes
  SameExecutionsInIf.Tests.testAll
  DefaultComesLast.Tests.testAll
  SimplifyBooleanReturn.Tests.testAll
  NeedBraces.Tests.testAll
