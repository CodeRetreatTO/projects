import Data.List as List
import Data.String as Str

completions :: String -> ([String] -> [String])
completions partial = filter (isPrefixOf partial)

main = do 
  wordlist <- readFile "/usr/share/dict/american-english"
  mapM_ print . completions "ther" $ Str.lines wordlist
  mapM_ print . completions "foo" $ Str.lines wordlist                      
