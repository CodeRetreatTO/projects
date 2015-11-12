import Test.Hspec

type Point = (Float, Float)
type Polygon = [Point]


square :: Polygon
square = [ (0,0), (0,1), (1,1), (1,0) ]

triangle :: Polygon
triangle = [ (0, 0), (2, 0), (1, sqrt 3) ]

star :: Polygon
star = [ (4,0), (1,1), (0,4), (-1,1), (-4,0), (-1,-1), (0,-4), (1,-1) ]

inside :: Polygon -> Point -> Point -> Bool
inside polygon p1 p2 = True


angle here prev next =
  let a = line here prev
      b = line here next
   in acos ( dotProduct a b / (len a * len b) )


len (x,y) = sqrt (x*x + y*y)
line (x1,y1) (x2,y2) = (x2-x1,y2-y1)
dotProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2


angleInside :: Point -> Point -> Point -> Point -> Bool
angleInside prev next here target =
  let a1 = angle here prev next
      a2 = angle here prev target
   in a2 < a1


main = hspec $ do
  describe "inside" $ do
    specify "square"   $ inside square (0,0) (1,1) `shouldBe` True
    specify "triangle H" $ inside triangle (0,0) (2,0) `shouldBe` True
    specify "triangle V" $ inside triangle (1,0) (1,sqrt 3) `shouldBe` True
    specify "star H" $ inside triangle (-4,0) (0,4) `shouldBe` True
    specify "star D" $ inside triangle (4,0) (0,-4) `shouldBe` False

  describe "angle" $ do
    specify "90deg" $ angle (0,0) (1,0) (0,1) `shouldBe` pi / 2
    specify "90deg translated" $ angle (1,1) (2,1) (1,2) `shouldBe` pi / 2
    specify "45deg" $ angle (0,0) (1,0) (1,1) - pi / 4 `shouldSatisfy` (< 0.01)

  describe "angleInside" $ do
    specify "ex1" $ angleInside (1,0) (0,1) (0,0) (1,1) `shouldBe` True
    specify "ex2" $ angleInside (1,0) (0,1) (0,0) (-1,1) `shouldBe` False
    specify "ex3" $ angleInside (1,0) (0,1) (0,0) (-1,0) `shouldBe` False


