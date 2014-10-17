module SeamCarving where

import Data.Char (ord)
import Data.List (sortBy)
import Data.Function (on)

type Score = [[Integer]]

scoreLine :: String -> [Integer]
scoreLine line = map total zipped
    where l = map (toInteger . ord) line
          zipped = zip3 (head l:l) l $ tail l
          total (a, b, c) = abs (a-b) + abs (b-c)

scoreGrid :: [String] -> Score
scoreGrid ls = map scoreLine ls

maskSeam :: [String] -> Int -> [String]
maskSeam lns ix = map (replace ' ' ix) lns

carveSeam :: [String] -> Int -> [String]
carveSeam lns ix = map (remove ix) lns

freshSeams :: [Integer] -> [Seam]
freshSeams ln = map (\(ix, w) -> Seam w [ix]) $ zip [1..] ln

seamsIn :: Score -> [Seam]
seamsIn [] = []
seamsIn s = sortByWeight allSeams
    where allSeams = foldl (\memo ln -> line ln memo) (freshSeams $ head s) $ tail s
          line ln seams = concatMap (take 1) $ map (\(ix, w) -> choose ix w seams) $ zip [0..] ln
          choose ix w seams = map (\s -> add s w ix) . take 1 . sortByWeight $ potentials ix seams
          potentials ix seams = take 3 $ drop (max 0 (fromIntegral ix-1)) seams

sortByWeight :: [Seam] -> [Seam]
sortByWeight = sortBy (compare `on` weight)

main :: IO ()
main = do f <- fmap lines $ readFile "scene.txt"
          mapM_ putStrLn $ maskSeam f 3
--          putStrLn $ show $ scoreGrid f

----- Seams
data Seam = Seam { weight :: Integer, sIxs :: [Integer ] } deriving (Eq, Ord, Show) 

add :: Seam -> Integer -> Integer -> Seam
add (Seam w ixs) newWeight newIx = Seam (w + newWeight) $ newIx:ixs

indices :: Seam -> [Integer]
indices (Seam _ ixs) = reverse ixs

----- Utility
remove :: (Num i, Enum i, Eq i) => i -> [a] -> [a]
remove _ [] = []
remove 0 (_:rest) = rest
remove ix (a:rest) = a : (remove (pred ix) rest)

replace :: (Num i, Enum i, Eq i) => a -> i -> [a] -> [a]
replace _ _ [] = []
replace new 0 (_:rest) = new:rest
replace new ix (a:rest) = a : (replace new (pred ix) rest)
