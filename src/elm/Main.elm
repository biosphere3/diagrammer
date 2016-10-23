module Main exposing (..)

import Debug exposing (log)
import Html.App as App
import Html exposing (Html)
import Dict exposing (Dict)
import Init exposing (init)
import State exposing (update, subscriptions)
import View.ViewField
import View.ViewUI


view model =
    Html.div
        []
        [ View.ViewField.view model
        , View.ViewUI.view model
        ]


main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
