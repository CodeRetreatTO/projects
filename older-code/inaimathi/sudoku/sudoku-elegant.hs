-- Pilfered (but modified) from http://web.math.unifi.it/users/maggesi/haskell_sudoku_solver.html
import Data.List

type Board = (Int,Int) -> [Int]

main :: IO ()
main = putStr $ unlines $ map disp $ solve [input sampleFiendish]

----- Take a list of boards and return a list of solutions
solve :: [Board] -> [Board]
solve boards = foldr search boards idx

search :: (Int, Int) -> [Board] -> [Board]
search (x, y) boards = [mark ((x, y), val) b | b <- boards, val <- b (x, y)]

----- Take a (x, y)
mark :: ((Int,Int),Int) -> Board -> Board
mark (a@(x,y), val) board b@(x',y')
  | a == b = 
    [val]
  | x' == x || y' == y || (e x' x) && (e y' y) =
    delete val $ board b
  | otherwise = 
    board b
  where e a b = div (a-1) 3 == div (b-1) 3

----- Converting [[Int]] to ... something using mark
----- Looks like this is building up thunks? So that the result is going to be a function that takes a particular (x, y) tuple and
----- returns a list of possibilities for that board square, passing it through all previous deletions?
input :: [[Int]] -> Board
input s = foldr mark (const [1..9]) $                                  -- (const [1..9]) is equivalent to (\_ -> [1..9])
          [((x, y), val) | ((x, y), val) <- zip idx $ concat s, val>0] -- 0 is the empty value

----- Print a board
disp :: Board -> String
disp s  = unlines [unwords [show $ head $ s (i,j) | j <- [1..9]] | i <- [1..9]]

----- Each grid x,y pair
idx :: [(Int,Int)]
idx = [(x,y) | y <- [1..9], x <- [1..9]]

sample :: [[Int]]
sample = [[0,7,1,4,0,0,0,0,5],
          [0,0,0,0,5,0,0,8,0],
          [0,0,3,9,0,7,6,0,0],
          [0,0,0,0,0,1,0,0,0],
          [0,9,0,8,0,6,0,0,3],
          [0,0,0,0,0,0,8,2,0],
          [0,6,0,0,4,0,7,0,8],
          [3,0,0,0,0,0,0,9,0],
          [0,0,0,0,8,5,0,0,0]]

sampleHard :: [[Int]]
sampleHard = [[0,7,1,4,0,0,0,0,5],
              [0,0,0,0,5,0,0,8,0],
              [0,0,3,9,0,7,6,0,0],
              [0,0,0,0,0,1,0,0,0],
              [0,9,0,0,0,6,0,0,3],
              [0,0,0,0,0,0,8,2,0],
              [0,0,0,0,4,0,0,0,8],
              [3,0,0,0,0,0,0,9,0],
              [0,0,0,0,8,5,0,0,0]]

sampleFiendish :: [[Int]]
sampleFiendish = [[0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,6,0,0,3],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0,0,0]]