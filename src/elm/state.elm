module State exposing (..)

import Focus exposing (..)
import Math.Vector2 exposing (..)
import Mouse
import Time exposing (..)

import Dict exposing (..)
import Foci exposing (..)
import Model exposing (..)
import Util exposing (..)


textOffsetRate = 1.0

type Msg
    = DragStart Draggable Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | MouseWheelTurn Mouse.Position (Int, Int) Float

    | SetEpoch Int

    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


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
            DragScreen ->
              Focus.update (globalTransform => translate) (add (dragOffset drag)) model'
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
                    addFlow (container, dragJack) model'
                  Nothing ->
                    if List.length jackHits > 0
                    then
                      let
                        jacks = (dragJack :: jackHits)
                        newContainer =
                          let
                            newID = nextID model.containerByID
                            newPos = centroid <| List.map .position jacks
                            containerName = "(" ++ dragJack.name ++ ")"
                          in Container newID containerName newPos (100, 100)

                        model'' =
                          { model'
                          | containerByID = Dict.insert newContainer.id newContainer containerByID
                          }
                        pairs : List (Container, Jack)
                        pairs = List.map ((,) newContainer) jacks

                        model''' = List.foldl
                          addFlow
                          model''
                          pairs
                      in
                        model'''
                    else
                      model'

    MouseWheelTurn position (width, height) delta ->
      let
        ratio = Debug.log "delta" <| 1 + delta / 100
      in
        Focus.update globalTransform (\xf -> {xf | scale = xf.scale * ratio}) model

    SetEpoch epoch ->
      { model | epoch = epoch }

    Tick t ->
      let
        u _ flow = { flow | textOffset = flow.textOffset + textOffsetRate }
        flowByID' = flowByID |> Dict.map u
      in
        { model | flowByID = flowByID' }


addFlow' : (Container, Jack) -> Model -> (Model, Flow)
addFlow' (container, jack) model =
  let
    id = nextID model.flowByID
    flow =
      { id = id
      , containerID = container.id
      , jackID = jack.id
      , textOffset = -2000
      }
    flowByID' = Dict.insert flow.id flow model.flowByID
    model' = { model | flowByID = flowByID' }
  in
    (model', flow)

addFlow : (Container, Jack) -> Model -> Model
addFlow pair model = fst <| addFlow' pair model

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    dragging =
      case model.drag of
        Nothing -> Sub.batch
          [ Mouse.downs (DragStart DragScreen) ]

        Just _ -> Sub.batch
          [ Mouse.moves DragAt
          , Mouse.ups DragEnd ]
    ticking = every (33 * millisecond) Tick
  in
    Sub.batch
      [ dragging
      --, ticking
      ]


nextID : Dict comparable { a | id : ID } -> ID
nextID dict =
  dict
  |> Dict.values
  |> List.map .id
  |> List.maximum
  |> Maybe.withDefault 0
  |> (+) 1


rectsCollide : Physical a -> Physical b -> Bool
rectsCollide a b =
  let
    (w1, h1) = a.rect
    (w2, h2) = b.rect
    d = a.position `sub` b.position
    (dx, dy) = toTuple d
  in
    abs dx < (w1 + w2) / 2 && abs dy < (h1 + h2) / 2


jackCollisions : Model -> Jack -> List Jack
jackCollisions model jack =
  let
    jacks = Dict.values model.jackByID
    hit = \j -> j.id /= jack.id && (rectsCollide jack j)
  in
    List.filter hit jacks

jackContainerCollision : Model -> Jack -> Maybe Container
jackContainerCollision model jack =
  let
    containers : List Container
    containers = Dict.values model.containerByID

    hit : Container -> Bool
    hit = rectsCollide jack
  in
    List.head <| List.filter hit containers

getJacks : Model -> Process -> List Jack
getJacks {jackByID} process =
  jackByID
  |> Dict.values
  |> List.filter (\x -> x.processID == process.id)


joinJacks : Model -> List Jack -> Model
joinJacks model jacks = model
  -- add a link between these jacks
