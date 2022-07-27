import EmptyLoopBody.Tests (testAllEmptyLoopBodies)
import NoLoopBreak.Tests (testAllNoLoopBreaks)

main :: IO ()
main = do
  testAllNoLoopBreaks
  testAllEmptyLoopBodies
