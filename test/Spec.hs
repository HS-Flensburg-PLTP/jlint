import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NeedBraces.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  NeedBraces.Tests.testAll
