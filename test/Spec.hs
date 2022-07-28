import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import SameExecutionsInIf.Tests

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  SameExecutionsInIf.Tests.testAll
