module Anagrams where

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Char as Chr

-- import qualified Debug.Trace as Dbg

dict :: IO [String]
dict = fmap lines $ readFile "/usr/share/dict/american-english"

type Gram = Vector Int

empty :: Gram
empty = Vec.replicate 26 0

decode :: String -> Gram
decode str = Vec.accum (+) empty charMap
    where  charMap = filter (relevant . fst) $ map (paired . Chr.toUpper) str
           relevant c = and [25 >= c, c >= 0]
           paired c = (Chr.ord c - 65, 1)

fits :: Gram -> Gram -> Bool
fits a b = and . Vec.toList $ Vec.zipWith (>=) a b

remove :: Gram -> Gram -> Gram
remove = Vec.zipWith (-)

allFits :: Gram -> [String] -> [String]
allFits w words = filter (fits w . decode) words

ana :: String -> [String] -> [[String]]
ana word dict = recur gram $ allFits gram dict
    where gram = decode word
          oneGrams g d = map (:[]) $ filter (\w -> empty == (remove g $ decode w)) $ d
          recur g d = concat [oneGrams g d, concatMap next d]
              where next w = map (w:) (recur rem $ allFits rem d)
                        where rem = remove g $ decode w
