import Ersatz.Counter.Common (parseCounter)
import Ersatz.Solution (CountResult(..))
import System.Exit (exitFailure, exitSuccess)

testCases :: [(String, CountResult)]
testCases =
  [ ("s mc 576\n", Count 576)
  , ("s mc 0\n", Count 0)
  , ("s mc 1234567890\n", Count 1234567890)
  , ("c comment\ns mc 42\n", Count 42)
  , ("s mc 100\ns mc 200\n", Count 100)
  , ("unrelated\ns mc 99\nmore stuff\n", Count 99)
  , ("s SATISFIABLE\n", CountUnsolved)
  , ("", CountUnsolved)
  , ("mc 100\n", CountUnsolved)
  , ("s other 100\n", CountUnsolved)
  , ("s mc\n", CountUnsolved)
  , ("s mc abc\n", CountUnsolved)
  ]

runTest :: (String, CountResult) -> IO Bool
runTest (input, expected) = do
  let result = parseCounter input
  if result == expected
    then do
      putStrLn "✓"
      return True
    else do
      putStrLn "✗"
      putStrLn $ "  Input: " ++ show input
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Got: " ++ show result
      return False

main :: IO ()
main = do
  results <- mapM runTest testCases
  let passed = length (filter id results)
      total = length results
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " tests passed"
  if Prelude.all id results
    then exitSuccess
    else exitFailure
