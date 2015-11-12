import Prelude hiding (iterate, cycle)
import Test.Hspec

type Point = (Double, Double)
type Polygon = [Point]


square :: Polygon
square = [ (0,0), (0,1), (1,1), (1,0) ]

triangle :: Polygon
triangle = [ (0, 0), (2, 0), (1, sqrt 3) ]

triangle2 :: Polygon
triangle2 = [ (0,0), (1,0), (1,1) ]
triangle2a = [ (0,0), (sqrt 2, 0), (sqrt 2 / 2, sqrt 2 / 2) ]
triangle2b = [ (0,0), (1,0), (0,1) ]

boundingBox :: Polygon -> (Point, Point)
boundingBox polygon = ((minX, minY), (maxX, maxY))
  where minX = minimum xs
        minY = minimum ys
        maxX = maximum xs
        maxY = maximum ys
        xs = map fst polygon
        ys = map snd polygon

rotations :: Polygon -> [Polygon]
rotations poly = map rotateToAxis (iterate (length poly) cycle poly)

iterate :: Int -> (a -> a) -> a -> [a]
iterate 0 f a = []
iterate n f a = a : iterate (n-1) f (f a)

cycle :: [a] -> [a]
cycle [] = []
cycle (x:xs) = xs ++ [x]

rotateToAxis :: Polygon -> Polygon
rotateToAxis poly = twist (move poly)
  where
    (x1,y1) = first poly
    (x2,y2) = second poly
    move poly = translate (-x1, -y1) poly
    twist poly = rotate theta poly where theta = negate (angle (x1,y1) (x2,y2) (1,0))

translate (x,y) poly = map f poly where f (x',y') = (x+x', y+y')
rotate theta poly = map f poly
  where
    f (x,y) = (x*cos theta - y*sin theta, x*sin theta + y*cos theta)

angle here prev next =
  let a = line here prev
      b = line here next
   in acos ( dotProduct a b / (len a * len b) )


len (x,y) = sqrt (x*x + y*y)
line (x1,y1) (x2,y2) = (x2-x1,y2-y1)
dotProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2

first = head
second = head . tail


main = hspec $ do
  describe "boundingBox" $ do
    specify "square" $ boundingBox square `shouldBe` ((0,0), (1,1))
    specify "triangle" $ boundingBox triangle `shouldBe` ((0,0), (2,sqrt 3))

  describe "rotations" $ do
    specify "square" $ rotations square `shouldBe` [square, square, square, square]
    specify "triangle" $ rotations triangle `shouldBe` [triangle, triangle, triangle]
    specify "triangle2" $ rotations triangle2 `shouldBe` [triangle2, triangle2a, triangle2b]


