import Math.Lattices.Fplll.LLL
import System.Exit

testStr = show defaultLLL

main = do
  putStrLn testStr
  putStrLn $ show $ lllReduce defaultLLL [[0, 0, 1], [1, 1, 0], [1, 2, 1]]
  exitSuccess
