import Debug exposing (log)
import Html.App as App

import Dict exposing (Dict)

import Init exposing (init)
import State exposing (update, subscriptions)
import View exposing (view)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
