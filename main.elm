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

update (NewContents reddit) oldContent =
  let
    output =
      (compile reddit (Array.initialize 4 (always 0)) 0 "")
  in
    { model | content = output }
    
safeget ptr memory =
  case Array.get ptr memory of
    Nothing -> 1
    Just x -> x

compile string memory ptr output =
   case String.uncons string of
    Nothing -> output
    Just (a, b) -> 
      case a of 
        '+' -> compile b (Array.set ptr ((safeget ptr memory) + 1) memory) ptr output
        '-' -> compile b (Array.set ptr ((safeget ptr memory) - 1) memory) ptr output
        '>' -> compile b memory (ptr + 1) output
        '<' -> compile b memory (ptr - 1) output
        '.' -> compile b memory ptr (output ++ (String.cons 
          (Char.fromCode (safeget ptr memory)) ""))
        _ -> compile b memory ptr output

-- VIEW

view tamale =
  div []
    [ input [ placeholder "Text to reverse", onInput NewContents, myStyle ] []
    , div [ myStyle ] [ text tamale.content ]
    ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
