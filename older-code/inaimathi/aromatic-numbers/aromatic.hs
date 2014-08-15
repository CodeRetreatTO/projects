import Data.Maybe
import Data.Char

lookRoman char = fromJust $ lookup char charTable
  where charTable = zip "IVXLCDM" [1, 5, 10, 50, 100, 500, 1000]

fromAromatic :: String -> Int
fromAromatic aromatic = rec aromatic 0
  where rec (a:r:a':r':rest) total = rec (a':r':rest) $ if r' > r then total - (prod a r) else total + (prod a r)
        rec (a:r:[]) total = total + (prod a r)
        prod a r = (digitToInt a) * (lookRoman r)