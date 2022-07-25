import CheckScope.Tests
import EmptyLoopBody.Tests

main :: IO ()
main = do
  testAllEmptyLoopBodies
  CheckScope.Tests.testAll
