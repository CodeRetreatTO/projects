module Autocomplete where

import String
import Keyboard
import Graphics.Input as Input

(field, content) = Input.field "Enter word."

fState : [String] -> Input.FieldState
fState comps = case comps of
                 [] -> {string = "", selectionStart=0, selectionEnd=0}
                 _  -> {string = (head comps), selectionStart=0, selectionEnd= (String.length <| head comps)}

esc = Keyboard.isDown 27
ctrlSpace = dropRepeats . lift and <| combine [Keyboard.ctrl, Keyboard.space]

empty : Signal Element
empty = sampleOn (merge Keyboard.enter esc) . fst <| Input.field "Enter text"

completeElem : Signal Element
completeElem = lift ((Input.fields Input.emptyFieldState).field id "Enter text") . sampleOn ctrlSpace . lift fState <| lift2 completions content wordList

completions : String -> [String] -> [String]
completions partial wordList = if | 0 < String.length partial -> filter (String.startsWith partial) wordList
                                  | otherwise          -> []

main = lift (flow down) <| combine [ lift plainText <| constant "Ctrl+space to complete, Enter to accept and Esc to clear."
                                   , lift (width 500) <| merges [field, completeElem, empty]
                                   , lift asText <| lift2 completions content wordList]

-- port wordList : Signal [String]

wordList = constant ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

port output : Signal String
port output = keepIf (\s -> s/="") "" <| sampleOn (keepWhen Keyboard.enter False Keyboard.enter) content
