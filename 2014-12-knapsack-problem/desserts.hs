module Desserts where

import Data.List (nub, sort)

type Menu = [(String, Int)]

desserts :: Menu
desserts = [ ("Mixed Fruit", 215)
           , ("French Fries", 275)
           , ("Side Salad", 335)
           , ("Hot Wings", 355)
           , ("Mozzarella Sticks", 420)
           , ("Sampler Plate", 580) ]

addItem menu o = map (\i -> i:o) menu

nextOrders menu os = concatMap (addItem menu) os

allOrders m = concat $ iterate (nextOrders m) $ addItem m []

orderTotal o = sum $ map snd o

canonicalize o = sort $ map fst o

filterFor menu money = nub . map canonicalize . filter ((==money) . orderTotal) $ allOrders menu

main money = head $ filterFor desserts money
