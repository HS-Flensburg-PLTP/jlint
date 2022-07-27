import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)

main :: IO ()
main = do
  testAllNoLoopBreaks
  testAllEmptyLoopBodies
  NamingConventions.Tests.testAll
