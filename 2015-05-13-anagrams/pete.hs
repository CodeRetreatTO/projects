import Data.List

-- Not the fastest solution, but probably the simplest...

dict :: IO [String]
dict = fmap (sortBy longestFirst . lines) $ readFile "/usr/share/dict/words"

longestFirst :: String -> String -> Ordering
longestFirst a b = compare (length b) (length a)

relevant :: String -> [String] -> [String]
relevant term = filter (containedIn term)
  where containedIn a b = sort a `intersect` sort b == sort b

anagrams :: String -> [String] -> [[String]]
anagrams "" dict = [[]]
anagrams term dict = concatMap recur $ relevant term dict
  where recur word = map (word :) $ anagrams (term \\ word) dict
