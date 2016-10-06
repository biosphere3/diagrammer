module View.ViewUI exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onMouseEnter, onMouseLeave)
import Json.Decode as Json exposing ((:=))
import Math.Vector2 as V2 exposing (..)
import Mouse

import Util exposing (..)
import Model exposing (..)
import State exposing (Msg(..))

(:>) = (,)

view : Model -> Html Msg
view model =
  let
    styles = style
      [ "width" :> "100px"
      , "height" :> "100%"
      , "position" :> "fixed"
      , "top" :> "0"
      , "left" :> "0"
      , "background" :> "white"
      , "border-right" :> "1px solid gray"
      ]
  in
    div
      [ styles
      ]
      [ b [] [text "hi"]
      ]
