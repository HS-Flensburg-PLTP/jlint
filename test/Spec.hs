import DefaultComesLast.Tests
import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NeedBraces.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import SimplifyBooleanReturn.Tests

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  DefaultComesLast.Tests.testAll
  SimplifyBooleanReturn.Tests.testAll
  NeedBraces.Tests.testAll
