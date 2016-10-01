module State exposing (..)

import Mouse exposing (Position)

import Dict exposing (..)
import Model exposing (..)
import Util exposing (..)
-- MODEL


type Msg
    = DragStart Draggable Position
    | DragAt Position
    | DragEnd Position


nextID : Dict comparable { a | id : ID } -> ID
nextID dict =
  dict
  |> Dict.values
  |> List.map .id
  |> List.maximum
  |> Maybe.withDefault 0
  |> (+) 1

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


-- UPDATE

shapesCollide : Physical a -> Physical b -> Bool
shapesCollide a b =
  case (a.shape, b.shape) of
    ((Circle r1), (Circle r2)) ->
      distanceSquared a.position b.position < r1 ^ 2 + r2 ^ 2
    ((Circle r), (Rect w h)) ->
      distance a.position b.position < (toFloat <| Basics.min w h)
    _ -> Debug.crash "TODO"


jackCollisions : Model -> Jack -> List Jack
jackCollisions model jack =
  let
    jacks = Dict.values model.jackByID
    hit = \j -> j.id /= jack.id && (shapesCollide jack j)
  in
    List.filter hit jacks

jackContainerCollision : Model -> Jack -> Maybe Container
jackContainerCollision model jack =
  let
    containers : List Container
    containers = Dict.values model.containerByID

    hit : Container -> Bool
    hit = shapesCollide jack
  in
    List.head <| List.filter hit containers


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )

getJacks : Model -> Process -> List Jack
getJacks {jackByID} process =
  jackByID
  |> Dict.values
  |> List.filter (\x -> x.processID == process.id)


joinJacks : Model -> List Jack -> Model
joinJacks model jacks = model
  -- add a link between these jacks


updateHelp : Msg -> Model -> Model
updateHelp msg ({processByID, jackByID, containerByID, flowByID, drag} as model) =
  case msg of
    DragStart target xy ->
      { model | drag = (Just (Drag xy xy target)) }

    DragAt xy ->
      { model | drag = (Maybe.map (\{start, target} -> Drag start xy target) model.drag) }

    DragEnd _ ->
      case drag of
        Nothing -> Debug.crash "not gonna happen"
        Just drag ->
          let
            model' = { model | drag = Nothing }
            updateProcessPosition process =
              { process | position = getProcessPosition model process }
            updateJackPosition jack =
              { jack | position = getJackPosition model jack }
            updateContainerPosition container =
              { container | position = getContainerPosition model container }
          in case drag.target of
            DragProcess dragProcess ->
              let
                attachedJacks = getJacks model dragProcess
              in
                { model'
                | processByID = processByID |> Dict.update dragProcess.id (Maybe.map updateProcessPosition)
                , jackByID = jackByID |> updateMulti (List.map .id attachedJacks) (Maybe.map updateJackPosition)
                }
            DragContainer dragContainer ->
              { model'
              | containerByID = containerByID |> Dict.update dragContainer.id (Maybe.map updateContainerPosition)
              }
            DragJack dragJack ->
              let
                jackHits = jackCollisions model (updateJackPosition dragJack)
                containerHit = jackContainerCollision model (updateJackPosition dragJack)
              in
                case containerHit of
                  Just container ->
                    let
                      nextFlowID = nextID flowByID
                    in
                      { model'
                      | flowByID = (Dict.insert nextFlowID (Flow nextFlowID container.id dragJack.id InFlow) flowByID)
                      }
                  Nothing ->
                    if List.length jackHits > 0
                    then
                      { model'
                      | jackByID = jackByID |> Dict.update dragJack.id (Maybe.map updateJackPosition)
                      }
                    else
                      model'



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    dragging =
      case model.drag of
        Nothing ->
          Sub.none

        Just _ ->
          Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
  in
    Sub.batch [ dragging ]
