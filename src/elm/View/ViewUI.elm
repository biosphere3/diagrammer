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


(:>) =
    (,)


view : Model -> Html Msg
view model =
    let
        styles =
            style
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
            , class "time-control"
            ]
            [ timeControl model
            ]


timeControl : Model -> Html Msg
timeControl ({ epoch, playing } as model) =
    let
        go n =
            SetEpoch <| n

        rewindAttr =
            if epoch > 0 then
                [ onClick (go 0) ]
            else
                [ disabled True ]

        prevAttr =
            if epoch > 0 then
                [ onClick (go <| epoch - 1) ]
            else
                [ disabled True ]

        playAttr =
            if not playing then
                [ onClick (SetPlaying True) ]
            else
                [ disabled True ]

        pauseAttr =
            if playing then
                [ onClick (SetPlaying False) ]
            else
                [ disabled True ]

        nextAttr =
            [ onClick (go <| epoch + 1) ]
    in
        div
            [ style
                [ "text-align" :> "center" ]
            ]
            [ div []
                [ button
                    rewindAttr
                    [ i [ class "fa fa-fast-backward" ] [] ]
                , button
                    prevAttr
                    [ i [ class "fa fa-step-backward" ] [] ]
                , button
                    pauseAttr
                    [ i [ class "fa fa-pause" ] [] ]
                , button
                    playAttr
                    [ i [ class "fa fa-play" ] [] ]
                , button
                    nextAttr
                    [ i [ class "fa fa-step-forward" ] [] ]
                , div
                    [ style
                        [ "display" :> "inline-block"
                        , "margin-left" :> "15px"
                        ]
                    ]
                    [ text "day: "
                    , text (toString model.epoch)
                    ]
                ]
            ]
