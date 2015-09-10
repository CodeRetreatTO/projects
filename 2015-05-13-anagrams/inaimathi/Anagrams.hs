module Anagrams where

import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.List (sortBy)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Char as Chr

type Gram = Vector Int
type Entry = String
type Dictionary = [Entry]

dict :: IO Dictionary
dict = fmap break $ readFile "/usr/share/dict/american-english"
    where break = sortBy (flip compare `on` length) . filter ((>2) . length) . lines
                  

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

relevant :: Gram -> Dictionary -> Dictionary
relevant w dict = filter (fits w . decode) dict

ana :: String -> Dictionary -> [String]
ana word dict = catMaybes $ recur [] gram $ relevant gram dict
    where gram = decode word
          recur _ _ [] = [Nothing]
          recur acc gram dict
              | empty == gram = [Just . unwords $ reverse acc]
              | otherwise = concatMap next dict
                where next w = let rem = remove gram $ decode w
                               in recur (w:acc) rem $ relevant rem dict

main :: IO ()
main = do d <- dict
          putStrLn . show . take 50 $ ana "School master" d
