import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import NamingConventions.Tests


main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
