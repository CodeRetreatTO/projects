module Santa where

import Data.List

names = ["Alex", "Beatrice", "Clarence", "Diego", "Ernest"] -- , "Francesca", "Gwen", "Heidi", "Ingrid"

-- lets_be_santa :: [String] -> [[(String, String)]]
lets_be_santa names = sequence . map (pair_with names) $ reverse names

pair_with :: [a] -> a -> [(a, a)]
pair_with lst elem = zip lst $ repeat elem

lets_be_picky_santa filter_fn names = take 1 . dropWhile filter_fn $ lets_be_santa names

is_selfie [] = False
is_selfie ((a, b):rest) 
    | a == b = True
    | otherwise = is_selfie rest 

is_multigiving pairs = (length pairs) /= (length . nub $ map fst pairs)

picky pairs = or [(is_selfie pairs), (is_multigiving pairs)]
