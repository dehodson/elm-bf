-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Array
import Char
import Debug

main =
  beginnerProgram { model = model, view = view, update = update }


-- UPDATE

type Msg = NewContents String

type alias Model =
  { content : String
  , input : String
  }
  
model : Model
model =
  Model "" "Said I calmed down since the last album"

update (NewContents program) oldContent =
  let
    output =
      (compile program (Array.initialize 256 (always 0)) 0 "" 0 model.input)
  in
    { model | content = output }
    
safeget ptr memory =
  case Array.get ptr memory of
    Nothing -> 1
    Just x -> x

find_end string pos balance =
  let
    character = String.slice pos (pos + 1) string
  in
    if pos == (String.length string) then
        String.length string
    else
      if character == "[" then
        find_end string (pos + 1) (balance + 1)
      else if character == "]" then
        if balance == 1 then
          (pos + 1)
        else
          find_end string (pos + 1) (balance - 1)
      else
        find_end string (pos + 1) balance

find_start string pos balance =
  let
    character = String.slice pos (pos + 1) string
  in
    if character == "[" then
      if balance == 0 then
        pos
      else
        find_start string (pos - 1) (balance + 1)
    else if character == "]" then
        find_start string (pos - 1) (balance - 1)
    else
      find_start string (pos - 1) balance

loop_start string memory ptr pos =
  if (safeget ptr memory) == 0 then
    find_end string (pos + 1) 1
  else
    (pos + 1)

loop_end string memory ptr pos =
  find_start string (pos - 1) 0

take_input string memory ptr output pos input =
  case String.uncons input of
    Just (a, b) -> 
      compile string (Array.set ptr (Char.toCode a) memory) ptr output (pos + 1) b
    _ -> compile string memory ptr output (pos + 1) input

compile string memory ptr output pos input =
  if pos == (String.length string) then
    output
  else
    case (String.slice pos (pos + 1) string) of 
      "+" -> compile string (Array.set ptr ((safeget ptr memory) + 1) memory) ptr output (pos + 1) input
      "-" -> compile string (Array.set ptr ((safeget ptr memory) - 1) memory) ptr output (pos + 1) input
      ">" -> compile string memory (ptr + 1) output (pos + 1) input
      "<" -> compile string memory (ptr - 1) output (pos + 1) input
      "." -> compile string memory ptr (output ++ (String.cons 
        (Char.fromCode (safeget ptr memory)) "")) (pos + 1) input
      "," -> take_input string memory ptr output pos input
      "[" -> compile string memory ptr output (loop_start string memory ptr pos) input
      "]" -> compile string memory ptr output (loop_end string memory ptr pos) input
      _ -> compile string memory ptr output (pos + 1) input

-- VIEW

view model =
  div []
    [ input [ placeholder "Program", onInput NewContents, myStyle ] []
    , div [ myStyle ] [ text model.content ]
    ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
