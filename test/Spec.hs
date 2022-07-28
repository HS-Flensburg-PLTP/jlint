import DefaultComesLast.Tests
import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  DefaultComesLast.Tests.testAll
