module DictRequest where

import Http

port output : Signal [String]
port output = lift reqLines . Http.sendGet <| constant "/elm-sample/dict.txt"

reqLines : Http.Response String -> [String]
reqLines req = case req of
                 Http.Success res -> String.lines res
                 _ -> []

