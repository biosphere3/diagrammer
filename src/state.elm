module State exposing (..)

import Mouse exposing (Position)

import Dict exposing (..)
import Model exposing (..)
import Util exposing (..)


type Msg
    = DragStart Draggable Position
    | DragAt Position
    | DragEnd Position



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
                    fst <| addFlow (container, dragJack) model'
                  Nothing ->
                    if List.length jackHits > 0
                    then
                      let
                        jacks = (dragJack :: jackHits)
                        newID = nextID model.containerByID
                        newPos = centroid <| List.map .position jacks
                        newContainer = Container newID "???" newPos (Rect 100 100)
                        model'' =
                          { model'
                          | containerByID = Dict.insert newContainer.id newContainer containerByID
                          }
                        pairs : List (Container, Jack)
                        pairs = List.map ((,) newContainer) jacks
                        add : (Container, Jack) -> Model -> Model
                        add x y = fst (addFlow x y)
                        model''' = List.foldl
                          add
                          model''
                          pairs
                      in
                        model'''
                    else
                      model'


addFlow : (Container, Jack) -> Model -> (Model, Flow)
addFlow (container, jack) model =
  let
    id = nextID model.flowByID
    flow = (Flow id container.id jack.id)
    flowByID' = Dict.insert flow.id flow model.flowByID
    model' = { model | flowByID = flowByID' }
  in
    (model', flow)


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


nextID : Dict comparable { a | id : ID } -> ID
nextID dict =
  dict
  |> Dict.values
  |> List.map .id
  |> List.maximum
  |> Maybe.withDefault 0
  |> (+) 1


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

getJacks : Model -> Process -> List Jack
getJacks {jackByID} process =
  jackByID
  |> Dict.values
  |> List.filter (\x -> x.processID == process.id)


joinJacks : Model -> List Jack -> Model
joinJacks model jacks = model
  -- add a link between these jacks
