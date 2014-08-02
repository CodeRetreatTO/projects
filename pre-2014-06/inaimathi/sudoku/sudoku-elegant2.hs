import Data.List
import Control.Monad

type Board = (Int,Int) -> [Int]

main = do
  boardString <- getContents
  putStr . unlines . map disp $ solve [input boardString]

solve :: [Board] -> [Board]
solve boards = foldr search boards idx where
  search (x, y) boards = [mark ((x, y),val) brd | brd <- boards, val <- brd (x, y)]

mark :: ((Int,Int),Int) -> Board -> Board
mark (p@(x,y),val) board p'@(x',y') 
  | p == p' = 
    [val]
  | x==x' || y==y' || blockBound x x' && blockBound y y' = 
    delete val $ board p'
  | otherwise =
    board p'
  where blockBound a b = div (a-1) 3==div (b-1) 3

disp :: Board -> String
disp board = unlines [unwords [show . head $ board (x,y) | y <- [1..9]] | x <- [1..9]]

input :: String -> Board
input boardString = foldr mark (const [1..9]) $
  [((x, y),val) | ((x, y),val) <- zip idx . map read $ lines boardString >>= words, val>0]

idx :: [(Int,Int)]
idx = [(x,y) | y <- [1..9], x <- [1..9]]