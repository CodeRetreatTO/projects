module Main where
import Data.List (group, sort, concatMap)
import Data.Set

inRange :: Ord a => a -> a -> a -> Bool
inRange low n high = low < n && n < high

lifeStep :: Int -> Set (Int, Int) -> Set (Int, Int)
lifeStep worldSize cells = fromList [head g | g <- grouped cells, viable g]
  where grouped = group . sort . concatMap neighbors . toList
        neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], 
                            (dx,dy) /= (0,0), inSize (dx+x) (dy+y)]
        inSize x y = inR x worldSize && inR y worldSize
        inR = inRange 0
        viable [_,_,_] = True
        viable [c,_] = c `member` cells
        viable _ = False

runLife :: Int -> Int -> Set (Int, Int) -> Set (Int, Int)
runLife worldSize steps cells = rec (steps - 1) cells
  where rec 0 cells = cells
        rec s cells = rec (s - 1) $! lifeStep worldSize cells

glider = fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = fromList [(1, 0), (1, 1), (1, 2)]
gosperGliderGun = fromList [(24, 0), (22, 1), (24, 1), (12, 2), (13, 2), (20, 2), (21, 2), (34, 2), (35, 2), (11, 3), (15, 3), (20, 3), (21, 3), (34, 3), (35, 3), (0, 4), (1, 4), (10, 4), (16, 4), (20, 4), (21, 4), (0, 5), (1, 5), (10, 5), (14, 5), (16, 5), (17, 5), (22, 5), (24, 5), (10, 6), (16, 6), (24, 6), (11, 7), (15, 7), (12, 8), (13, 8)]

main :: IO ()
main = putStrLn . show $ runLife 50 5000 gosperGliderGun