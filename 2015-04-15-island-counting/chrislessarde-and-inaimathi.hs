module IslandCounting where

type Coord = (Int, Int)
type Grid = [Coord]

testStrings :: [String]
testStrings = [ "......."
              , ".xx...."
              , "xxxx..."
              , ".xx...."
              , "....xx."
              , "...xxx."
              , "....x.."
              , "......."]

testGrid :: Grid
testGrid = [(0, 3), (0, 4), (0, 5), (16, 12)]

-- toGrid :: [String] -> Grid
-- toGrid ss = 

countIslands :: Grid -> Int
countIslands [] = 0
countIslands grid = 1 + (countIslands $ sinkOne grid)

sinkOne :: Grid -> Grid
sinkOne [] = []
sinkOne g = ablate g (head g)

ablate :: Grid -> Coord -> Grid
ablate g c = if c `elem` g 
             then foldl ablate removed ns
             else g
    where ns = filter (flip elem removed) $ neighbors c
          removed = filter (/=c) g

neighbors :: Coord -> [Coord]
neighbors (x, y) = [ (x-1, y-1), (x, y-1), (x+1, y-1)
                   , (x-1, y),             (x+1, y)
                   , (x-1, y+1), (x, y+1), (x+1, y+1)]

-- [(x'+x, y'+y) | x' <- [-1,0,1], y'<-[-1,0,1], (x',y') /= (0,0)]
