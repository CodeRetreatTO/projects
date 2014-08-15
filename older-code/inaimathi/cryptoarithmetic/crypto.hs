module Main where

import Data.List (nub)
import Data.Maybe (fromJust)

main = do
  mapM_ showSolved [("SM", sampleSM),
                    ("MD", sampleMD),
                    ("LG", sampleLG)]
  where showSolved (tag, phrase) = do
          putStrLn tag
          mapM_ (putStrLn . show) $ solve phrase

sampleSM = "IT IS OK"
sampleMD = "TWO TWO FOUR"
sampleLG = "THIS MAY JUST WORK"
sampleXLG = "COLORLESS GREEN IDEAS SLEEP FURIOUSLY"

solve phrase = [bind i | i <- [1 .. max], check (bind i) nonZero wds]
  where wds = words phrase
        nonZero = nub $ map head wds
        bind = toBindings letters
        letters = nub $ concat wds
        max = 10 ^ (length letters) - 1

toBindings :: [Char] -> Int -> [(Char, Int)]
toBindings letters int = rec letters int []
  where rec [] _ acc = acc
        rec rest 0 acc = acc ++ (zip rest $ repeat 0)
        rec (l:rest) i acc = rec rest (fromEnum $ i `div` 10) 
                             $ (l, fromEnum $ i `mod` 10):acc

check :: [(Char, Int)] -> [Char] -> [String] -> Bool
check bindings nonZero words = isSum && isUnique && isProper
  where vals = map (value bindings) words
        isSum = sum (init vals) == last vals
        isUnique = map snd bindings == (nub $ map snd bindings)
        isProper = all ((/=0) . fromJust . flip lookup bindings) nonZero

value :: [(Char, Int)] -> String -> Int
value bindings word = sum $ zipWith (*) nums [10^i | i <- [0..10]]
  where nums = reverse $ map (fromJust . flip lookup bindings) word