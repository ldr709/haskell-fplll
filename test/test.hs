import Math.Lattices.Fplll
import System.Exit

testStr = show ftLongDouble

main = do
  putStrLn testStr
  putStrLn $ show $ lllReduce defaultLLL [[0, 0, 1], [1, 1, 0], [1, 2, 1]]
  exitSuccess
