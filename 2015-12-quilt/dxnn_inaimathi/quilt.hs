module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  putSteps test
  putSteps screwTest
  putSteps bigTest
      where putSteps = putStrLn . show . take 1 . findSteps

findSteps :: Quilt -> [[(Vertex, Vertex)]]
findSteps q = recur [q]
    where recur [] = []
          recur qs
              | [] == filter isDone qs = recur $ concatMap nextSteps qs
              | otherwise = concat [map (reverse . steps) qs, recur $ filter (not . isDone) qs]

nextSteps :: Quilt -> [Quilt]
nextSteps q = catMaybes . map stitch . nub $ edges q
    where stitch (Edge _ a b) = combineVertices q a b

combineVertices :: Quilt -> Vertex -> Vertex -> Maybe Quilt
combineVertices q v v' = case nub . map dirOf . filter relevantE $ newEs of
                           [_] -> Just $ q { vertices = newVs
                                           , edges = filter (not . relevantE) $ newEs
                                           , steps = (v, v') : (steps q)}
                           _ -> Nothing
    where newVertex = v ++ v'
          newVs = newVertex : (filter (\e -> not $ e `elem` [v, v']) $ vertices q)
          newEs = map (replaceV v newVertex) . map (replaceV v' newVertex) $ edges q
          relevantE (Edge _ a b) = and [a == newVertex, b == newVertex]

---------- Datatypes
type Vertex = String
data Direction = U | O deriving (Eq, Show)
data Edge = Edge Direction Vertex Vertex deriving (Eq, Show)

replaceV :: Vertex -> Vertex -> Edge -> Edge
replaceV old new edge@(Edge dir a b)
    | and [a == old, b == old] = Edge dir new new
    | a == old = Edge dir new b
    | b == old = Edge dir a new
    | otherwise = edge

dirOf :: Edge -> Direction
dirOf (Edge d _ _) = d

data Quilt = Quilt { edges :: [Edge], vertices :: [Vertex], steps :: [(Vertex, Vertex)] } deriving (Eq, Show)

isDone :: Quilt -> Bool
isDone q = [] == edges q

---------- Test data
mkTest :: String -> [Edge] -> Quilt
mkTest vs es = Quilt { vertices = group vs, edges = es, steps = [] }

test :: Quilt
test = mkTest "abc" [Edge U "a" "b", Edge U "a" "c", Edge O "b" "c"]

bigTest :: Quilt
bigTest = mkTest "abcdefgh" [
           Edge O "a" "b", Edge U "a" "c"
          ,Edge U "b" "d", Edge U "b" "e"
          ,Edge O "c" "d", Edge O "c" "h", Edge U "c" "f", Edge U "c" "g"
          ,Edge U "d" "h", Edge O "d" "e"
          ,Edge U "e" "h"
          ,Edge O "f" "g"
          ,Edge O "g" "h"
          ]

screwTest :: Quilt
screwTest = mkTest "abcdef" [
             Edge O "a" "b", Edge U "a" "d", Edge U "a" "c", Edge O "a" "e"
            ,Edge U "b" "e"
            ,Edge O "c" "d", Edge O "c" "f"
            ,Edge O "d" "e", Edge U "d" "f"
            ,Edge U "e" "f"
            ]
