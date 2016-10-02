module Init exposing (..)


import Dict exposing (Dict)
import Math.Vector2 exposing (..)

import Model exposing (..)
import State exposing (Msg(..))
import Util exposing (..)

processes =
  [ { name = "Biodigester", position = vec2 100 120 }
  , { name = "Rainwater Catchment", position = vec2 300 400 }
  , { name = "Composting Toilet", position = vec2 500 200 }
  ]

jacks =
  [ { name = "Effluent", processID = 1, direction = Input }
  , { name = "Biogas", processID = 0, direction = Output }
  , { name = "Poo", processID = 2, direction = Input }
  ]

containers =
  [ { name = "Rain Barrel", position = vec2 100 600}
  ]

init : ( Model, Cmd Msg )
init =
  let
    processByID : ProcessDict
    processByID =
      processes |> (List.indexedMap mkProcess) |> toDictByID

    jackByID : JackDict
    jackByID =
      jacks |> (List.indexedMap (mkJack processByID)) |> toDictByID

    flowByID : FlowDict
    flowByID = Dict.empty
      --[] |> (List.indexedMap mkFlow) |> toDictByID

    containerByID : ContainerDict
    containerByID =
      containers |> (List.indexedMap mkContainer) |> toDictByID
  in (
    Model
      processByID
      jackByID
      containerByID
      flowByID
      Nothing
     , Cmd.none )



mkProcess id {name, position} = Process id name "" Nothing position (Rect 160 (160 + id))

mkContainer id {name, position} = Container id name position (Rect 160 160)

--mkFlow id {containerID, jackID, direction} = Flow containerID jackID direction

mkJack processByID id {name, processID, direction} =
  let
    process = seize processID processByID
    position = process.position `add` vec2 100 50
    shape = Circle jackRadius
  --in Jack id name processID 42.0 direction position shape
  in
    { id = id
    , name = name
    , processID = processID
    , rate = 42.0
    , direction = direction
    , position = position
    , shape = shape
    }
