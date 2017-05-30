-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Array
import Char

main =
  beginnerProgram { model = model, view = view, update = update }


-- UPDATE

type Msg = NewContents String

type alias Model =
  { content : String
  }
  
model : Model
model =
  Model ""

update (NewContents program) oldContent =
  let
    output =
      (compile program (Array.initialize 4 (always 0)) 0 "" 0)
  in
    { model | content = output }
    
safeget ptr memory =
  case Array.get ptr memory of
    Nothing -> 1
    Just x -> x

find_end string pos balance =
  let
    character = (String.slice pos (pos + 1) string)
  in
    if pos == (String.length string) then
        String.length string
    else
      if character == "[" then
        find_end string (pos + 1) (balance + 1)
      else if character == "]" then
        if balance == 1 then
          pos
        else
          find_end string (pos + 1) (balance - 1)
      else
        find_end string (pos + 1) balance

find_start string pos balance =
  let
    character = (String.slice pos (pos + 1) string)
  in
    if character == "[" then
      if balance == 1 then
        pos
      else
        find_start string (pos - 1) (balance + 1)
    else if character == "]" then
        find_end string (pos - 1) (balance - 1)
    else
      find_end string (pos - 1) balance

loop_start string memory ptr pos =
  if (safeget ptr memory) == 0 then
    find_end string (pos + 1) 1
  else
    (pos + 1)

loop_end string memory ptr pos =
  find_start string (pos - 1) 0

compile string memory ptr output pos =
  if pos == (String.length string) then
    output
  else
    case (String.slice pos (pos + 1) string) of 
      "+" -> compile string (Array.set ptr ((safeget ptr memory) + 1) memory) ptr output (pos + 1)
      "-" -> compile string (Array.set ptr ((safeget ptr memory) - 1) memory) ptr output (pos + 1)
      ">" -> compile string memory (ptr + 1) output (pos + 1)
      "<" -> compile string memory (ptr - 1) output (pos + 1)
      "." -> compile string memory ptr (output ++ (String.cons 
        (Char.fromCode (safeget ptr memory)) "")) (pos + 1)
      "[" -> compile string memory ptr output (loop_start string memory ptr (pos + 1))
      "]" -> compile string memory ptr output (loop_end string memory ptr (pos - 1))
      _ -> compile string memory ptr output (pos + 1)

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
