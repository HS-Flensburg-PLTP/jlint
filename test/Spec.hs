import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import NonFinalMethodAttributes.Tests (testAllNonFinalMethodAttributes)

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  testAllNonFinalMethodAttributes
