module Display where

port input : Signal String

main = sigList <| foldp (::) [] input

sigList : Signal [a] -> Signal Element
sigList = lift (flow down . map asText)
