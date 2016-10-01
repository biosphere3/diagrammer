module Init exposing (..)


import Dict exposing (Dict)
import Mouse exposing (Position)

import Model exposing (..)
import State exposing (Msg(..))
import Util exposing (..)


init : ( Model, Cmd Msg )
init =
  let
    processByID : ProcessDict
    processByID =
      [ { name = "Biodigester", position = Position 100 100 }
      , { name = "Can of Beans", position = Position 300 400 }
      , { name = "Can of Beans", position = Position 500 200 }
      ] |> (List.indexedMap mkProcess) |> toDictByID

    jackByID : JackDict
    jackByID =
      [ { name = "Effluent", processID = 1, direction = Input }
      , { name = "Biogas", processID = 2, direction = Output }
      ] |> (List.indexedMap (mkJack processByID)) |> toDictByID

    flowByID : FlowDict
    flowByID = Dict.empty
      --[] |> (List.indexedMap mkFlow) |> toDictByID

    containerByID : ContainerDict
    containerByID =
      [ { name = "Rain Barrel", position = Position 100 600}
      ] |> (List.indexedMap mkContainer) |> toDictByID
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
    process = case Dict.get processID processByID of
      Just process -> process
      Nothing -> (Debug.crash (toString processID))
    position = process.position /+/ Position 100 50
    shape = Circle jackRadius
  in Jack id name processID 42.0 direction position shape
