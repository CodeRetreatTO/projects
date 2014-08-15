{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.IxSet (Indexable, IxSet, Proxy(..), empty, size, ixSet, ixFun, fromList, toList, toAscList, delete, union, getOne, (@=), (@>), updateIx)
import Data.Data (Data, Typeable)
import Data.List (intercalate)
import qualified Data.List as Lst (delete)
import Data.List.Split (chunksOf)

--- Class definitions
---------- Basics (mostly for IxSet lookups)
newtype Block = Block { unBlock :: (Int, Int) } deriving (Eq, Ord, Show, Typeable)
newtype Length = Length Int deriving (Eq, Ord, Show, Typeable)
newtype X = X Int deriving (Eq, Ord, Show, Typeable)
newtype Y = Y Int deriving (Eq, Ord, Show, Typeable)

---------- Indexable Space
data Space = Space { coords :: (Int, Int), block :: Block, values :: [Int] }
           deriving (Eq, Ord, Typeable, Show)
instance Indexable Space where
  empty = ixSet [ iF $ coords
                , iF $ block
                , ixFun $ values
                , iF $ Length . length . values
                , iF $ X . fst . coords
                , iF $ Y . snd . coords
                ]
    where iF ix = ixFun $ (:[]) . ix

---------- A full Board
data Board = Board { spaces :: IxSet Space, width :: Int, bw :: Int } deriving (Eq)
instance Show Board where
  show board = '\n' : (unlines . intercalate hdelim . chunks $ [row y | y <- [1..width board]])
    where row y = intercalate "|" . chunks . concatMap spaceToStr $ find y
          find y = toList $ spaces board @= Y y
          chunks = chunksOf (bw board)
          hdelim = [replicate (width board + (bw board - 1)) '-']
          spaceToStr s = case values s of
            [val] -> show val
            _ -> " "

--- Functions
solve :: [Board] -> Board
solve boards = step boards [] []
  where step [] acc solved = step acc [] solved
        step (b:rest) acc solved = case minSpace b of 
          [min] -> step rest ((map (\v -> (insert b (coords min) v)) $ values min) ++ acc) solved
          [] -> b

naiveSolve :: ([a] -> a) -> Board -> Board
naiveSolve fn board = case minSpace board of
  [min] -> naiveSolve fn . insert board (coords min) . head $ values min
  [] -> board
          
countSpace :: Int -> Board -> Int
countSpace len board = size $ spaces board @= Length len

minSpace :: Board -> [Space]
minSpace board = take 1 . toAscList (Proxy :: Proxy Length) $ spaces board @> Length 1

insert :: Board -> (Int, Int) -> Int -> Board
insert board p@(x, y) val = upBoard filtered [toSpace p [val] $ bw board]
  where filtered = upBoard board $ map (\s -> s { values = Lst.delete val $ values s }) relevants
        Just space = getOne $ spaces board @= p
        relevants = toList $ delete space $ 
                    unions [spaces board @= Y y, 
                            spaces board @= X x, 
                            spaces board @= block space]

blankBoard :: Int -> Board
blankBoard width = Board { spaces = blanks, width = width, bw = bw }
  where blanks = fromList [toSpace (x, y) ixs bw | y <- ixs, x <- ixs]
        ixs = [1..width]
        bw = fromEnum . sqrt $ toEnum width

---------- Type converters/convenience functions
toBoard :: [[Int]] -> Board
toBoard vals = foldr into blank spaces
  where blank = blankBoard w
        into = (\(x, y, v) b -> insert b (x, y) v)
        spaces = [(x, y, look x y) | y <- ixs, x <- ixs, look x y /= 0]
        look x y = vals !! (y-1) !! (x-1)
        w = length $ vals !! 0
        ixs = [1..w]

toSpace :: (Int, Int) -> [Int] -> Int -> Space
toSpace p@(x, y) val bw = Space { coords = p, values = val, block = toBlock p bw }

toBlock :: (Int, Int) -> Int -> Block
toBlock (x, y) bw = Block { unBlock = (o x, o y) }
  where o n = div (n-1) bw

--- Utility
---------- Sudoku-specific utility
upBoard :: Board -> [Space] -> Board
upBoard board sps = board { spaces = updates coords (spaces board) sps }

---------- General utility
updates :: (Ord a, Typeable a, Typeable k, Indexable a) => (a -> k) -> IxSet a -> [a] -> IxSet a
updates fn set elems = foldr (\e memo -> updateIx (fn e) e memo) set elems

unions :: (Ord a, Typeable a, Indexable a) => [IxSet a] -> IxSet a
unions sets = foldl union empty sets


--- Sample Data
sample :: [[Int]]
sample = [[0,7,1,4,0,0,0,0,5],
          [0,0,0,0,5,0,0,8,0],
          [0,0,3,9,0,7,6,0,0],
          [0,0,0,0,0,1,0,0,0],
          [0,9,0,8,0,6,0,0,3],
          [0,0,0,0,0,0,8,2,0],
          [0,6,0,0,4,0,7,0,8],
          [3,0,0,0,0,0,0,9,0],
          [0,0,0,0,8,5,0,0,0]]

sampleHard :: [[Int]]
sampleHard = [[0,7,1,4,0,0,0,0,5],
              [0,0,0,0,5,0,0,8,0],
              [0,0,3,9,0,7,6,0,0],
              [0,0,0,0,0,1,0,0,0],
              [0,9,0,0,0,6,0,0,3],
              [0,0,0,0,0,0,8,2,0],
              [0,0,0,0,4,0,0,0,8],
              [3,0,0,0,0,0,0,9,0],
              [0,0,0,0,8,5,0,0,0]]

sampleFiendish :: [[Int]]
sampleFiendish = [[0,7,1,4,0,0,0,0,0],
                  [0,0,0,0,5,0,0,0,0],
                  [0,0,3,9,0,7,6,0,0],
                  [0,0,0,0,0,0,0,0,0],
                  [0,9,0,0,0,6,0,0,3],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,4,0,0,0,8],
                  [0,0,0,0,0,0,0,0,0],
                  [0,0,0,0,8,5,0,0,0]]