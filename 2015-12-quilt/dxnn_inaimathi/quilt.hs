import Data.List

type Vertex = String
data Edge = Edge Vertex Vertex deriving (Eq, Show)
data Quilt = Quilt { edges :: [Edge], vertices :: [Vertex], steps :: [(Vertex, Vertex)] } deriving (Show)

test :: Quilt
test = Quilt {
         vertices = ["a", "b", "c"]
       , edges = [Edge "a" "b", Edge "a" "c", Edge "b" "c"]
       , steps = []
       }

bigTest :: Quilt
bigTest = Quilt {
            vertices = ["a", "b", "c", "d", "e", "f", "g", "h"]
          , edges = [
             Edge "a" "b", Edge "a" "c"
            ,Edge "b" "d", Edge "b" "e"
            ,Edge "c" "d", Edge "c" "h", Edge "c" "f", Edge "c" "g"
            ,Edge "d" "h", Edge "d" "e"
            ,Edge "e" "h"
            ,Edge "f" "g"
            ,Edge "g" "h"
            ]
          , steps = []
          }

combineVertices :: Quilt -> Vertex -> Vertex -> Quilt
combineVertices q v v' = q { vertices = newVs, edges = newEs }
    where newVertex = v ++ v'
          newVs = newVertex : (filter (\e -> not $ e `elem` [v, v']) $ vertices q)
          newEs = filter (/= (Edge newVertex newVertex)) . map replaceAs . map replaceBs $ edges q
          replaceAs e@(Edge a b)
              | a `elem` [v, v'] = Edge newVertex b
              | otherwise = e
          replaceBs e@(Edge a b)
              | b `elem` [v, v'] = Edge a newVertex
              | otherwise = e

nextSteps :: Quilt -> [Quilt]
nextSteps q = map stitch . nub $ edges q
    where stitch (Edge a b) = res { steps = (a, b) : (steps res)}
              where res = combineVertices q a b

findSteps :: Quilt -> [(Vertex, Vertex)]
findSteps q = recur [q]
    where recur qs = case filter (\q' -> [] == edges q') qs of
                       [] -> recur $ concatMap nextSteps qs
                       qs' -> reverse . steps $ head qs'
