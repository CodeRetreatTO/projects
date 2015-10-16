module Elections where

import Data.List (sortBy)

type Candidate = String
type Ballot = [(Int, Candidate)]

comp :: (Int, Candidate) -> (Int, Candidate) -> Ordering
comp (a, _) (b, _) = b `compare` a

testVote :: [Ballot]
testVote = [ [(2, "A"), (1, "B"), (0, "C")]
           , [(1, "A"), (2, "B"), (0, "C")]
           , [(1, "A"), (0, "B"), (2, "C")]
           ]

initialTotal = [(0, "A"), (0, "B"), (0, "C")]

borda :: [Ballot] -> Candidate
borda ballots = snd . head . sortBy comp $ foldl combine initialTotal ballots
    where combine total ballot = flip zip candidates $ zipWith (+) (ranks total) (ranks ballot)
          ranks = map fst
          candidates = map snd initialTotal
