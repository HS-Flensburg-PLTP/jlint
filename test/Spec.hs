import EmptyLoopBody.Tests
import UnnecessaryVariables.Tests (testAllUnnecessaryVariables)

main :: IO ()
main = do
  testAllEmptyLoopBodies
  testAllUnnecessaryVariables
