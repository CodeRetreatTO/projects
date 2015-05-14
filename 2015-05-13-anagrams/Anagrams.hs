module Anagrams where

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Char as Chr

dict :: IO [String]
dict = fmap lines $ readFile "/usr/share/dict/american-english"

type Gram = Vector Int

decode :: String -> Gram
decode str = Vec.accum (+) em charMap
    where  em = Vec.replicate 26 0
           charMap = filter (relevant . fst) $ map (paired . Chr.toUpper) str
           relevant c = and [25 >= c, c >= 0]
           paired c = (Chr.ord c - 65, 1)

fits :: Gram -> Gram -> Bool
fits a b = and . Vec.toList $ Vec.zipWith (>=) a b

remove :: Gram -> Gram -> Gram
remove = Vec.zipWith (-)

allFits :: Gram -> [String] -> [String]
allFits w words = filter (fits w . decode) words

-- anagramify :: String -> [String] -> [String]
anagramify w words = map (\target -> target : (allFits (remove gram $ decode target) reduced)) $ reduced
    where gram = decode w
          reduced = allFits gram words
          
