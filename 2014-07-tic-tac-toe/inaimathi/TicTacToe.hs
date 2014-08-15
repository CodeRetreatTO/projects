module TicTacToe where

import Data.List
import Data.Function (on)

data Player = X | O deriving (Eq, Read, Show)
next X = O
next O = X

type Space = Maybe Player
data GameState = Winner Player
               | Tie
               | Continue deriving (Eq, Read, Show)

data Game = Game Board [Game] deriving (Eq, Read, Show)

data Board = Board { grid :: [[Space]], width :: Int } deriving (Eq, Read)

play p board
    | board == empty w = head $ drop ((w ^ 2) `div` 2) allMs
    | gameState board == Continue = head moves
    | otherwise = board
    where w = width board
          allMs = allMoves p board
          moves = concat [filter ((==Winner p) . gameState) allMs
                         , sortBy pref allMs]
          pref = compare `on` (preference 5 p . gameTree)

allMoves :: Player -> Board -> [Board]
allMoves player board = recur [] (concat $ grid board) []
    where recur prefix (Nothing:rest) acc = recur (Nothing:prefix) rest $ placed:acc
              where placed = board { grid = sliceAt w $ reverse prefix ++ (Just player:rest)}
          recur prefix (s:rest) acc = recur (s:prefix) rest acc
          recur _ [] acc = acc
          w = width board

empty :: Int -> Board
empty w = Board { grid = n $ n Nothing, width = w}
    where n = replicate w

countBlanks :: Board -> Int
countBlanks board = length . filter (==Nothing) . concat $ grid board

----- Game Tree
gameTree :: Board -> Game
gameTree board = recur O board 
    where recur p board
              | gameState board /= Continue = Game board []
              | otherwise = Game board $ map (recur other) $ allMoves other board
                            where other = next p

preference :: Int -> Player -> Game -> Int
preference depth p game = sum $ levelOrderDepth depth game score
    where score b d
              | state == Winner opponent = -1 * d ^ 3
              | state == Winner p = d ^ 2
              | otherwise = 0
              where state = gameState b
          opponent = next p

levelOrderDepth :: Int -> Game -> (Board -> Int -> a) -> [a]
levelOrderDepth depth tree fn = recur depth [tree]
    where recur 0 _ = []
          recur _ [] = []
          recur d (Game b [] : rest) = fn b d : recur (pred d) rest
          recur d (Game b gs : rest) = fn b d : (recur (pred d) $ rest ++ gs)

----- Game-state checking
gameState :: Board -> GameState
gameState board = case (winners, countBlanks board) of
                    ([], 0) -> Tie
                    ([], _) -> Continue
                    ((Just w:_), _) -> Winner w
    where winners = concat . filter indecisive . map nub $ concatMap ($ board) [diagonals, columns, rows]
          indecisive [Nothing] = False
          indecisive [w] = True
          indecisive _ = False

diagonals :: Board -> [[Space]]
diagonals b = [map ix $ zip [0..len] [0..len], map ix $ zip [len,pred len..0] [0..len]]
    where len = pred . length $ grid b
          ix (x, y) = grid b !! y !! x

columns :: Board -> [[Space]]
columns = transpose . grid

rows :: Board -> [[Space]]
rows = grid

----- Display
instance Show Board where
    show b = concat [ "[", contents, "]"]
        where contents = intercalate "|" $ map (unwords . map showSpace) $ grid b
              showSpace Nothing = "_"
              showSpace (Just p) = show p

showBoard b = '\n' : (intercalate separator $ lines)
    where lines = map showLine $ grid b
          showLine = intercalate "|" . map showSpace
          showSpace Nothing = " "
          showSpace (Just p) = show p
          separator = concat ["\n", replicate sepLen '-',"\n"]
          sepLen = length . showLine . head $ grid b

----- Utility
sliceAt n [] = []
sliceAt n lst = head:tail
    where head = take n lst 
          tail = sliceAt n $ drop n lst

computeFor player board = zip moves $ map (preference 5 player . gameTree) moves 
    where moves = allMoves player board
