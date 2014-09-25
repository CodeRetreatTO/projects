module Main where

import Prelude hiding (Left, Right)

import Haste
import Haste.Concurrent
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

data Ant = Ant Point Direction deriving (Eq, Show)
data World = World [Ant] Coords deriving (Show)
type Coords = Set Point

data Point = Point Int Int deriving (Eq, Show)

instance Ord Point where
    (Point x y) `compare` (Point x' y') = case x `compare` x' of
                                            EQ -> y `compare` y'
                                            res -> res
    min (Point x y) (Point x' y') = Point (min x x') (min y y')
    max (Point x y) (Point x' y') = Point (max x x') (max y y')

bordersOf :: World -> (Point, Point)
bordersOf (World ants coords) = (min lo lo', max hi hi')
    where (lo, hi) = Set.foldl (\(lo, hi) pt -> (min lo pt, max hi pt)) zero coords
          (lo', hi') = foldl (\(lo, hi) (Ant pt _) -> (min lo pt, max hi pt)) zero ants
          zero = ((Point 0 0), (Point 0 0))

flipCell :: Point -> Coords -> Coords
flipCell cell coords= if cell `member` coords
                      then cell `delete` coords
                      else cell `insert` coords

turn :: Coords -> Ant -> Ant
turn coords (Ant pt dir) = Ant pt $ if pt `member` coords
                                    then left dir
                                    else right dir

onwards :: Ant -> Ant
onwards (Ant (Point x y) Up) = Ant (Point x (pred y)) Up
onwards (Ant (Point x y) Right) = Ant (Point (succ x) y) Right
onwards (Ant (Point x y) Down) = Ant (Point x (succ y)) Down
onwards (Ant (Point x y) Left) = Ant (Point (pred x) y) Left

step :: World -> World
step (World ants coords) = World newAnts newCoords
    where newCoords = foldl (\memo (Ant pt _) -> flipCell pt memo) coords ants
          newAnts = map (onwards . turn coords) ants


----- Pretty-print a world state
showWorld :: World -> String
showWorld world@(World ants coords) = unlines [line y | y <- [minY..maxY]]
    where line y = [charOf (Point x y) | x <- [minX..maxX]]
          ((Point minX minY), (Point maxX maxY)) = bordersOf world
          antCells = Map.fromList $ map (\(Ant pt dir) -> (pt, dir)) ants
          charDir Up = '↑'
          charDir Right = '→'
          charDir Down = '↓'
          charDir Left = '←'
          charOf cell
              | cell `Map.member` antCells = charDir . fromJust $ Map.lookup cell antCells
              | cell `member` coords = 'O'
              | otherwise = ' '

----- Utility functions
setContent :: ElemID -> String -> IO ()
setContent id newContent = withElem id (\e -> setProp e "innerHTML" newContent)

loop :: Monad m => (a -> m a) -> a -> m b
loop f a = do res <- f a
              loop f res

----- Test data and main
test :: World
test = World [(Ant (Point 4 4) Up), (Ant (Point 3 7) Left), (Ant (Point 15 12) Down)] $ fromList []

server :: IO ()
server = mapM_ print . take 20 $ iterate step test
    where print w = do putStr $ showWorld w
                       putStrLn "--------------------"

client :: IO ()
client = concurrent $ loop nextFrame (test, 0)
    where nextFrame (w, ct) = do liftIO $ setContent "world" $ showWorld w
                                 liftIO $ setContent "generations" $ show ct
                                 wait 50
                                 return $ (step w, succ ct)

main = client
