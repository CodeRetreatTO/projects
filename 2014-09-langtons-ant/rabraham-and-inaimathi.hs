module Main where

import Prelude hiding (Left, Right)

import Haste
import Data.Set (Set(..), member, insert, delete, fromList)
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Map as Map

data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant (Int, Int) Direction deriving (Eq, Show)
data World = World [Ant] Coords deriving (Show)
type Coords = Set (Int, Int)

ptMin (x, y) (x', y') = (min x x', min y y')
ptMax (x, y) (x', y') = (max x x', max y y')

bordersOf :: World -> ((Int, Int), (Int, Int))
bordersOf (World ants coords) = (ptMin min min', ptMax max max')
    where (min, max) = Set.foldl (\(min, max) pt -> (ptMin min pt, ptMax max pt)) zero coords
          (min', max') = foldl (\(min, max) (Ant pt _) -> (ptMin min pt, ptMax max pt)) zero ants
          zero = ((0, 0), (0, 0))

flipCell :: (Int, Int) -> Coords -> Coords
flipCell cell coords= if cell `member` coords
                  then cell `delete` coords
                  else cell `insert` coords

turn :: Coords -> Ant -> Ant
turn coords (Ant pt dir) = Ant pt $ if pt `member` coords
                                    then left dir
                                    else right dir

onwards :: Ant -> Ant
onwards (Ant (x, y) Up) = Ant (x, (pred y)) Up
onwards (Ant (x, y) Right) = Ant ((succ x), y) Right
onwards (Ant (x, y) Down) = Ant (x, (succ y)) Down
onwards (Ant (x, y) Left) = Ant ((pred x), y) Left

step :: World -> World
step (World ants coords) = World newAnts newCoords
    where newCoords = foldl (\memo (Ant pt _) -> flipCell pt memo) coords ants
          newAnts = map (onwards . turn coords) ants


----- Pretty-print a world state
showWorld :: World -> String
showWorld world@(World ants coords) = unlines [line y | y <- [minY..maxY]]
    where line y = [charOf (x, y) | x <- [minX..maxX]]
          ((minX, minY), (maxX, maxY)) = bordersOf world
          antCells = Map.fromList $ map (\(Ant pt dir) -> (pt, dir)) ants
          charDir Up = '↑'
          charDir Right = '→'
          charDir Down = '↓'
          charDir Left = '←'
          charOf cell
              | cell `Map.member` antCells = charDir . fromJust $ Map.lookup cell antCells
              | cell `member` coords = 'O'
              | otherwise = ' '

----- Haste stuff
setContent :: ElemID -> String -> IO ()
setContent id newContent = withElem id (\e -> setProp e "innerHTML" newContent)

animate :: Int -> World -> Int -> IO ()
animate delay world steps = setTimeout delay $ recur world steps
    where puts ct w = do setContent "world" $ showWorld w
                         setContent "generations" $ show (steps - ct)
          recur world 0 = setTimeout delay $ puts 0 world
          recur world ct = do puts ct world
                              setTimeout delay $ recur (step world) $ pred ct


----- Test data and main
test :: World
test = World [(Ant (4, 4) Up), (Ant (3, 7) Left), (Ant (15, 12) Down)] $ fromList []

serverDemo :: IO ()
serverDemo = mapM_ print . take 20 $ iterate step test
    where print w = do putStr $ showWorld w
                       putStrLn "--------------------"

main :: IO ()
main = animate 10 test 4000
