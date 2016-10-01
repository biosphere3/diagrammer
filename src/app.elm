import Debug exposing (log)
import Html.App as App

import Dict exposing (Dict)

import State exposing (init, update, subscriptions)
import View exposing (view)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- VIEW
