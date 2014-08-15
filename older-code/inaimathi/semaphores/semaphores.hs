module Semaphores where

import Data.List
import Data.Maybe

data Color = A | B | C | D deriving (Eq, Ord, Show, Enum)
type Symbol = (Color, Integer)

alphabet = [(color, num) | num <- [0 .. 59], color <- [A .. D]]

dictionary = [ (A, [0, 0])
             , (B, [0, 1])
             , (C, [1, 0])
             , (D, [1, 1])]

decode :: [Integer] -> [Symbol]
decode message = rec message A []
    where rec (a:b:rest) _ acc
               | and [a `elem` [0, 1], b `elem` [0, 1]] = rec rest (look [a, b]) acc 
          rec (num:rest) mode acc = rec rest mode $ (mode, num):acc
          rec [] _ acc = reverse acc
          look  = fromJust . flip lookup (map swap dictionary)

testMessage :: [Symbol]
testMessage = decode [0, 0, 50, 50, 52, 18, 1, 0, 19, 0, 1, 10, 1, 0, 47, 29, 10, 1, 1, 48]

encode :: [Symbol] -> [Integer]
encode message = concat listed
    where grouped = groupBy (\a b -> (fst a) == (fst b)) message
          lettered = map (\lst -> ((fst $ head lst), map snd lst)) grouped
          listed = map (\(clr, lst) -> ((look clr) ++ lst)) lettered
          look = fromJust . flip lookup dictionary 

swap (a, b) = (b, a)

coolEncode message = concat . map (\(clr, lst) -> ((fromJust $ flip lookup dictionary clr) ++ lst)) . map (\lst -> ((fst $ head lst), map snd lst)) $ groupBy (\a b -> (fst a) == (fst b)) message
