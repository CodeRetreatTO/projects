module Main where

import Prelude hiding (Left, Right)

import Haste
import Data.Set (Set(..), member, insert, delete, fromList)

data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant Int Int Direction deriving (Eq, Show)
data World = World [Ant] Coords deriving (Show)
type Coords = Set (Int, Int)

flipCell :: (Int, Int) -> Coords -> Coords
flipCell cell coords= if cell `member` coords
                  then cell `delete` coords
                  else cell `insert` coords

turn :: Coords -> Ant -> Ant
turn coords (Ant x y dir) = Ant x y $ if (x, y) `member` coords
                                      then left dir
                                      else right dir

onwards :: Ant -> Ant
onwards (Ant x y Up) = Ant x (pred y) Up
onwards (Ant x y Right) = Ant (succ x) y Right
onwards (Ant x y Down) = Ant x (succ y) Down
onwards (Ant x y Left) = Ant (pred x) y Left

step :: World -> World
step (World ants coords) = World newAnts newCoords
    where newCoords = foldl (\memo (Ant x y _) -> flipCell (x, y) memo) coords ants
          newAnts = map (onwards . turn coords) ants

test = World [(Ant 4 4 Up), (Ant 3 7 Left), (Ant 27 34 Down)] $ fromList []

showWorld :: (Int, Int) -> World -> String
showWorld (w, h) (World ants coords) = unlines [line y | y <- [0..h]]
    where line y = [charOf (x, y) | x <- [0..w]]
          antCells = map (\(Ant x y _) -> (x, y)) ants
          charOf cell
              | cell `elem` antCells = '+'
              | cell `member` coords = 'O'
              | otherwise = ' '

setContent :: ElemID -> String -> IO ()
setContent id newContent = withElem id (\e -> setProp e "innerHTML" newContent)

animate :: Int -> (Int, Int) -> World -> Int -> IO ()
animate delay size world steps = setTimeout delay $ recur world steps
    where puts ct w = do setContent "world" $ showWorld size w
                         setContent "generations" $ show (steps - ct)
          recur world 0 = setTimeout delay $ puts 0 world
          recur world ct = do puts ct world
                              setTimeout delay $ recur (step world) $ pred ct

main :: IO ()
main = animate 50 (50, 50) test 4000
