module View.ViewUI exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Json exposing (field)
import Math.Vector2 as V2 exposing (..)
import Mouse
import Util exposing (..)
import Model exposing (..)
import State exposing (Msg(..))
import String


(:>) =
    (,)


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "edit-panel" ]
            [ editForm model ]
        , div
            [ class "main-ui" ]
            [ settingsControl model
            , aboutPanel
            , timeControl model
            ]
        ]


settingsControl : Model -> Html Msg
settingsControl model =
    div
        [ class "settings-control" ]
        [ text "Show inputs/outputs"
        , input
            [ type_ "checkbox"
            , checked model.jacksVisible
            , onClick (SetJacksVisible <| not model.jacksVisible)
            ]
            []
        , div [ style [ "fontSize" :> "0.9em", "color" :> "gray" ] ] [ text "(shortcut: Alt+h)" ]
        ]


aboutPanel =
    div
        [ class "about-panel" ]
        [ a [ href "http://biosphere3.org" ]
            [ text "about" ]
        , text " :: "
        , a [ href "http://github.com/biosphere3/diagrammer" ]
            [ text "code" ]
        ]


editForm model =
    let
        ( form, panelStyle ) =
            case model.selected of
                Just (SelectedContainer id) ->
                    case Dict.get id model.containerByID of
                        Nothing ->
                            ( [], [] )

                        Just { name, capacity } ->
                            let
                                setName =
                                    (\x -> EditContainer id x capacity)

                                setCapacity x =
                                    case String.toFloat x of
                                        Ok cap ->
                                            EditContainer id name cap

                                        Err _ ->
                                            Noop

                                panelStyle =
                                    [ "left" :> "0" ]

                                form =
                                    [ div
                                        [ class "edit-form" ]
                                        [ h3 [] [ text "Edit Container" ]
                                        , label
                                            []
                                            [ text "Name"
                                            , input [ type_ "text", value name, onInputStop setName ] []
                                            ]
                                        , label
                                            []
                                            [ text "Capacity"
                                            , input [ type_ "text", value (toString capacity), onInputStop setCapacity ] []
                                            ]
                                        , div
                                            [ style
                                                [ "margin-top" :> "50px"
                                                , "font-size" :> "0.9em"
                                                ]
                                            ]
                                            [ ul []
                                                [ li [ class "delete-instruction" ] [ text "Double click container to delete" ]
                                                , li [ class "delete-instruction" ] [ text "Double click any link to delete" ]
                                                ]
                                            ]
                                        ]
                                    ]
                            in
                                ( form, panelStyle )

                _ ->
                    ( [], [] )
    in
        div
            [ class "edit-panel", style panelStyle ]
            form


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
            [ class "time-control"
            , style
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
