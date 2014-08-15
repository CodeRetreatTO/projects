module ThreeDee where

-- Editors Note:
--   This includes the very beginnings of a display system
--  that was going to call out to a web-app to display results
--  using some 3D javascript library.
--   So the post and paramsToString functions aren't strictly
--  speaking part of the solution.

import Data.List
import Network.HTTP -- import Network.HTTP.Conduit
-- Look into http://www.wxs.ca/js3d/
import Text.JSON

still :: [(Int, Int, Int)]
still = [(0, 0, 0), (1, 0, 0), (2, 0, 0),
         (0, 1, 0), (1, 1, 0), (2, 1, 0),
         (0, 2, 0), (1, 2, 0), (2, 2, 0)]

neighbors (x, y, z) = [(x + a, y + b, z + c) | a <- ixs, b <- ixs, c <- ixs, (a, b, c) /= (0, 0, 0)]
  where ixs = [-1, 0, 1]

step board = [head c | c <- cells, rules board c]
  where cells = group . sort . concat $ map neighbors board

rules :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
rules board [_, _, _, _, _] = True
rules _ _ = False

post :: String -> [(String, String)] -> Request_String
post uri params = postRequestWithBody uri "application/x-www-form-urlencoded" $ paramsToString params

paramsToString :: [(String, String)] -> String
paramsToString params = intercalate "&" $ map pair params
  where pair (k, v) = k ++ "=" ++ v
