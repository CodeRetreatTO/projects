import System.Random 
import Data.List

data Res = Ok | In | Out deriving (Eq, Show)

test = solve (check secret) . take 10 $ repeat [1..40]

secret :: [Int]
secret = [ 5, 26, 37, 2, 1, 7, 4, 38, 3, 7 ]

check :: [Int] -> [Int] -> [(Int, Res)]
check secret guess = map checkNum $ zip secret guess
  where checkNum (s, g)
          | s == g = (g, Ok)
          | elem g secret = (g, In)
          | otherwise = (g, Out)

-- solve :: ([Int] -> [Res]) -> [[Int]] -> [[Int]]
solve oracle possibilities = reducePos [] possibilities . oracle $ take 10 possibleNums 
  where possibleNums = nub $ concat possibilities
        reducePos acc (p:pos) ((g,r):res) 
          | r == Ok = reducePos ([g]:(removes g acc)) pos res
          | r == In = reducePos ((remove g p):acc) pos res
          | r == Out = reducePos ((remove g p):(removes g acc)) (removes g pos) res
        reducePos acc [] a = reverse acc
        reducePos acc a [] = reverse acc

remove :: Int -> [Int] -> [Int]
remove num list = filter (/=num) list

removes :: Int -> [[Int]] -> [[Int]]
removes num lists = map (remove num) lists