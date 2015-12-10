module Main where

import Data.List
import Data.Maybe

type Vertex = String
data Direction = U | O deriving (Eq, Show)
data Edge = Edge Direction Vertex Vertex deriving (Eq, Show)
data Quilt = Quilt { edges :: [Edge], vertices :: [Vertex], steps :: [(Vertex, Vertex)] } deriving (Show)

dirOf :: Edge -> Direction
dirOf (Edge d _ _) = d

test :: Quilt
test = Quilt {
         vertices = ["a", "b", "c"]
       , edges = [Edge U "a" "b", Edge U "a" "c", Edge O "b" "c"]
       , steps = []
       }

bigTest :: Quilt
bigTest = Quilt {
            vertices = ["a", "b", "c", "d", "e", "f", "g", "h"]
          , edges = [
             Edge O "a" "b", Edge U "a" "c"
            ,Edge U "b" "d", Edge U "b" "e"
            ,Edge O "c" "d", Edge O "c" "h", Edge U "c" "f", Edge U "c" "g"
            ,Edge U "d" "h", Edge O "d" "e"
            ,Edge U "e" "h"
            ,Edge O "f" "g"
            ,Edge O "g" "h"
            ]
          , steps = []
          }

combineVertices :: Quilt -> Vertex -> Vertex -> Maybe Quilt
combineVertices q v v' = case nub . map dirOf . filter relevantE $ newEs of
                           [_] -> Just $ q { vertices = newVs, edges = filter (not . relevantE) $ newEs }
                           _ -> Nothing
    where newVertex = v ++ v'
          newVs = newVertex : (filter (\e -> not $ e `elem` [v, v']) $ vertices q)
          newEs = map replaceAs . map replaceBs $ edges q
          relevantE (Edge _ a b)
              | and [a == newVertex, b == newVertex] = True
              | otherwise = False
          replaceAs e@(Edge dir a b)
              | a `elem` [v, v'] = Edge dir newVertex b
              | otherwise = e
          replaceBs e@(Edge dir a b)
              | b `elem` [v, v'] = Edge dir a newVertex
              | otherwise = e

nextSteps :: Quilt -> [Quilt]
nextSteps q = catMaybes . map stitch . nub $ edges q
    where stitch (Edge _ a b) = case res of
                                  Just r -> Just $ r { steps = (a, b) : (steps r)}
                                  Nothing -> Nothing
              where res = combineVertices q a b

findSteps :: Quilt -> [(Vertex, Vertex)]
findSteps q = recur [q]
    where recur qs = case filter (\q' -> [] == edges q') qs of
                       [] -> recur $ concatMap nextSteps qs
                       qs' -> reverse . steps $ head qs'
