import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NamingConventions.Tests
import NoLoopBreak.Tests (testAllNoLoopBreaks)
import NonPrivateAttributes.Tests (testAllNonPrivateAttributes)

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllNoLoopBreaks
  NamingConventions.Tests.testAll
  testAllNonPrivateAttributes
