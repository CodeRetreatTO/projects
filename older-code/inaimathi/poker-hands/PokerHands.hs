module PokerHands where

import Data.String
import Data.List
import Data.Ord

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine 
          | Ten | Jack | Queen | King | Ace 
          deriving (Eq, Ord, Show, Bounded, Enum)
                   
instance Read Rank where
  readsPrec _ value =
    let tbl = zip "23456789TJQKA" [Two .. Ace]
    in case lookup (head value) tbl of
      Just val -> [(val, tail value)]
      Nothing -> error $ "\nInvalid rank: " ++ value

data Suit = H | C | D | S deriving (Eq, Ord, Show, Read)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Ord, Show)

instance Read Card where
  readsPrec _ value =    
    [(Card (read r :: Rank) (read s :: Suit), drop 2 value)]
    where r = init value
          s = snd $ splitAt (length r) value
          
data Hand = Hand { handRank :: HandRank, cards :: [Card] } 
          deriving (Eq, Show, Ord)

instance Read Hand where
  readsPrec _ value =
    [(Hand (getHandRank res) res, "")]
    where res = reverse . sort . map read $ words value :: [Card]

data HandRank = HighCard [Rank] 
              | Pair [Rank]
              | TwoPair [Rank]
              | ThreeOfAKind [Rank]
              | Straight [Rank]
              | Flush [Rank]
              | FullHouse [Rank] 
              | FourOfAKind [Rank] 
              | StraightFlush [Rank]
              deriving (Eq, Ord, Show)

isFlush :: [Card] -> Bool
isFlush cards = (1==) . length . group $ map suit cards

isStraight :: [Card] -> Bool
isStraight cards = 
  let rs = sort $ map rank cards
      run = [(head rs) .. (last rs)]
  in rs == run

getHandRank cards =
  let ranks = reverse . sort $ map rank cards
      uniqueRanks = nub ranks
      rankGroups = sortByLen $ group ranks
      handRank = case cards of
        _ | (isFlush cards) && (isStraight cards)  -> StraightFlush
          | has4 rankGroups                        -> FourOfAKind
          | (has3 rankGroups) && (has2 rankGroups) -> FullHouse
          | isFlush cards                          -> Flush
          | isStraight cards                       -> Straight
          | has3 rankGroups                        -> ThreeOfAKind 
          | (countGroupsOf 2 rankGroups) == 2      -> TwoPair
          | has2 rankGroups                        -> Pair
          | otherwise                              -> HighCard
  in handRank uniqueRanks

-------------------------------
-- General Utility Functions --
-------------------------------
hasGroupOf :: Int -> [[a]] -> Bool
hasGroupOf n groups = n `elem` (map length groups)
has4 = hasGroupOf 4
has3 = hasGroupOf 3
has2 = hasGroupOf 2

countGroupsOf :: Int -> [[a]] -> Int
countGroupsOf n groups = length $ filter (\g -> length g == n) groups

sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy (flip $ comparing length)
