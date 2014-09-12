module Langton where

import Prelude hiding (Left, Right, flip)

data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant Int Int Direction deriving (Eq, Show)
data World = World Ant Coords deriving (Show)
type Coords = [(Int, Int)]


flip :: (Int, Int) -> Coords -> Coords
flip cell coords= if cell `elem` coords
                  then filter (/=cell) coords
                  else cell:coords

turn :: Ant -> Coords -> Ant
turn (Ant x y dir) coords = Ant x y $ if (x, y) `elem` coords
                                      then left dir
                                      else right dir

onwards :: Ant -> Ant
onwards (Ant x y Up) = Ant x (pred y) Up
onwards (Ant x y Right) = Ant (succ x) y Right
onwards (Ant x y Down) = Ant x (succ y) Down
onwards (Ant x y Left) = Ant (pred x) y Left

step :: World -> World
step (World ant@(Ant x y _) coords) = World newAnt newCoords
    where newCoords = flip (x, y) coords
          newAnt = onwards $ turn ant coords

test = World (Ant 4 4 Up) []

main = mapM_ (putStrLn . show) . take 400 $ iterate step test
