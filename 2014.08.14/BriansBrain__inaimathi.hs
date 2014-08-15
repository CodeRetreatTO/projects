{-# LANGUAGE DeriveDataTypeable #-}

module BriansBrain where

import Data.Data (Data, Typeable)
import Data.List (nub, groupBy, sortBy)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.IxSet (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, fromList, toList, toAscList)

data State = Off | Recovering | On deriving (Eq, Ord, Show, Typeable)
data Coords = Coords { x :: Int, y :: Int } deriving (Eq, Ord, Show, Typeable)
data Cell = Cell { coords :: Coords, state :: State } deriving (Eq, Ord, Show, Typeable)
instance Indexable Cell where
    empty = ixSet [ ixFun $ (:[]) . coords
                  , ixFun $ (:[]) . state ]

type Grid = IxSet Cell
type Rules = (Cell -> [Cell] -> Maybe Cell)

osc :: Grid
osc = fromList [ Cell (Coords 2 1) Recovering, Cell (Coords 2 2) On, Cell (Coords 3 2) On
               , Cell (Coords 4 2) Recovering, Cell (Coords 1 3)  Recovering, Cell (Coords 2 3) On, Cell (Coords 3 3) On
               , Cell (Coords 3 4) Recovering]

neighbors :: Grid -> Cell -> [Cell]
neighbors grid (Cell (Coords x y) _) = map cellAt cs
    where cs = [Coords (x'+x) (y'+y) | x' <- [-1..1], y' <- [-1..1], (x', y') /= (0, 0)]
          cellAt c = case getOne $ grid @= c of
                       Just cell -> cell 
                       Nothing -> Cell c Off

briansRules (Cell coords On) _ = Just $ Cell coords Recovering
briansRules (Cell _ Recovering) _ = Nothing
briansRules (Cell coords Off) ns = case filter (==On) $ map state ns of
                                     [_, _] -> Just $ Cell coords On
                                     _ -> Nothing

showGrid :: Grid -> String
showGrid g = unlines $ map showLine lines
    where desc = sortBy (onY compare) $ toList g
          lines = groupBy (onY (==)) desc
          onY = (`on` (y . coords))

showLine :: [Cell] -> String
showLine ln = recur ln 0 ""
    where recur [] _ acc = reverse acc
          recur ln@((Cell (Coords x _) state):rest) ct acc
              | x == ct = recur rest (succ ct) $ (showState state):acc
              | otherwise = recur ln (succ ct) $ ' ':acc

showState Off = ' '
showState On = 'X'
showState Recovering = 'O'

next :: Rules -> Grid -> Grid
next fn grid = fromList $ catMaybes $ map (\c -> fn c (neighbors grid c)) potentials 
    where potentials = nub $ concatMap (neighbors grid) $ toList grid

main = mapM_ print $ take 10 $ iterate (next briansRules) osc
    where print g = do putStr $ showGrid g
                       putStrLn "------------------------------"
