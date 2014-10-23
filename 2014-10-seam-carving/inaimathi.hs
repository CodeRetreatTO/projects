module SeamCarving where

import Data.Char (ord)
import Data.List (sortBy)
import Data.Function (on)

scoreGrid :: Image -> Score
scoreGrid ls = map scoreLine ls
    where scoreLine ln = map total zipped
              where l = map (toInteger . ord) ln
                    zipped = zip3 (head l:l) l $ tail l
                    total (a, b, c) = abs (a-b) + abs (b-c)

seamsIn :: Score -> [Seam]
seamsIn [] = []
seamsIn s = sortByWeight allSeams
    where allSeams = foldl line (freshSeams $ head s) $ tail s
          line seams ln = concatMap (choose seams) $ zip [0..] ln
          choose seams (ix, w) = map (\seam -> add seam w ix) . take 1 . sortByWeight $ potentials ix seams
          potentials ix seams = filter (\(Seam _ (i:_)) -> i >= ix-1 && ix+1 >= i) seams

scaleBy :: Image -> Int -> Image
scaleBy pic count = head $ drop count $ iterate scaleOne pic
    where scaleOne lns = case seamsIn $ scoreGrid lns of
                           [] -> lns
                           (cheapest:_) -> carveSeam lns cheapest

maskSeam :: Image -> Seam -> Image
maskSeam = applySeam (replace ' ')

carveSeam :: Image -> Seam -> Image
carveSeam = applySeam remove

freshSeams :: [Integer] -> [Seam]
freshSeams ln = map (\(ix, w) -> Seam w [ix]) $ zip [1..] ln

main :: IO ()
main = do f <- fmap lines $ readFile "scene.txt"
          mapM_ putBlock [ f
                         , scaleBy f 10
                         , scaleBy f 30
                         , scaleBy f 50 ]

----- Types and related minutia
type Score = [[Integer]]
type Image = [String]

data Seam = Seam { weight :: Integer, sIxs :: [Integer] } deriving (Eq, Ord, Show) 

add :: Seam -> Integer -> Integer -> Seam
add (Seam w ixs) newWeight newIx = Seam (w + newWeight) $ newIx:ixs

indices :: Seam -> [Integer]
indices (Seam _ ixs) = reverse ixs

----- Utility
putBlock :: [String] -> IO ()
putBlock lns = do mapM_ putStrLn lns
                  putStrLn " "

applySeam :: (Integer -> String -> String) -> Image -> Seam -> Image
applySeam fn lns seam = map (\(ix, ln) -> fn ix ln) $ zip (indices seam) lns

sortByWeight :: [Seam] -> [Seam]
sortByWeight = sortBy (compare `on` weight)

remove :: (Num i, Enum i, Eq i) => i -> [a] -> [a]
remove _ [] = []
remove 0 (_:rest) = rest
remove ix (a:rest) = a : (remove (pred ix) rest)

replace :: (Num i, Enum i, Eq i) => a -> i -> [a] -> [a]
replace _ _ [] = []
replace new 0 (_:rest) = new:rest
replace new ix (a:rest) = a : (replace new (pred ix) rest)
