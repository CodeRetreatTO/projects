import Data.List (filter, nub, isPrefixOf, sort, tails, inits, sortBy)
import Data.String (words)
import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (liftM)

raw :: IO [String]
raw = liftM (words . map toLower . filter (not . flip elem ",\".:;!?()=+_")) $ readFile "big.txt"

dict :: IO (Set.Set String)
dict = liftM Set.fromList raw

tally :: [String] -> Map.Map String Int
tally (w:ws) = incf w $ tally ws
    where incf word map = case Map.lookup word map of
                            Just ct -> Map.adjust (+1) word map
                            Nothing -> Map.insert word 1 map
tally [] = Map.empty

wordCount :: IO (Map.Map String Int)
wordCount = liftM tally raw

editDistanceOne :: String -> [String]
editDistanceOne word = Set.elems $ Set.unions $ map Set.fromList [ deletes, transposes, replaces, inserts ]
    where alpha = ['a'..'z']
          splits = zip (inits word) (tails word)
          deletes = [ concat [a, tail b] | (a, b) <- splits, b /= ""]
          transposes = [ concat [a, [b !! 1], [b !! 0], tail $ tail b] | (a, b) <- splits, 1 < length b]
          replaces = [ concat [a, [c], tail b] | (a, b) <- splits, c <- alpha, b /= "" ]
          inserts = [ concat [a, [c], b] | (a, b) <- splits, c <- alpha ]

editDistance :: Int -> String -> [String]
editDistance distance word = concat $ take (distance + 1) $ iterate (concatMap editDistanceOne) [word]

byCount :: Map.Map String Int -> [String] -> [String]
byCount ct words = reverse $ sortBy (\a b -> check a `compare` check b) words
    where check w = Map.findWithDefault 0 w ct
                   

known :: Set.Set String -> [String] -> [String]
known dict words = [ w | w <- words, Set.member w dict ]

correct :: Set.Set String -> Map.Map String Int -> String -> [String]                   
correct dict ct word = byCount ct . nub . known dict $ editDistance 2 word

complete :: Set.Set String -> Map.Map String Int -> String -> [String]
complete dict ct partial = byCount ct . filter (isPrefixOf partial) $ Set.toList dict

compleck :: Set.Set String -> Map.Map String Int -> String -> [String]
compleck dict ct word = take 16 . nub $ concat [ known dict [word], complete dict ct word, correct dict ct word ]
