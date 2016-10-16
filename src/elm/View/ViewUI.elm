module View.ViewUI exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
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
      [ "width" :> "100%"
      , "height" :> "40px"
      , "position" :> "fixed"
      , "top" :> "0"
      , "left" :> "0"
      , "padding" :> "15px"
      , "background" :> "white"
      , "box-shadow" :> "0 1px 10px rgba(0, 0, 0, 0.5)"
      --, "border-bottom" :> "1px solid gray"
      ]
  in
    div
      [ styles
      ]
      [ timeControl model
      ]

timeControl : Model -> Html Msg
timeControl ({epoch} as model) =
  let
    go n = SetEpoch <| model.epoch + n
    styles = style ["fontSize" :> "20px"]
    prevAttr =
      styles ::
      if epoch > 0
      then [onClick (go -1)]
      else [disabled True]
    nextAttr = [styles, onClick (go 1)]
  in
    div
      [styles]
      [ button
          prevAttr
          [text "<"]
      , text (toString model.epoch)
      , button
          nextAttr
          [text ">"]
      ]