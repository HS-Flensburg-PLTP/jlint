import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import UnnecessaryVariables.Tests (testAllUnnecessaryVariables)

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllUnnecessaryVariables
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
