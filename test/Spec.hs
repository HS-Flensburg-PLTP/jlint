import EmptyLoopBody.Tests
import NamingConventions.Tests

main :: IO ()
main = do
  testAllEmptyLoopBodies
  NamingConventions.Tests.testAll
