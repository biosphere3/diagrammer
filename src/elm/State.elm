module State exposing (..)

import Focus exposing (..)
import Math.Vector2 exposing (..)
import Mouse
import Time exposing (..)

import Calc
import Dict exposing (..)
import Foci exposing (..)
import Model exposing (..)
import String
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
updateHelp msg ({processByID, jackByID, containerByID, linkByID, drag} as model) =
  let
    {getProcess, getJack, getContainer, getLink} = getGetters model
  in case msg of
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
                    addLink (container, dragJack) model'
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
                          in
                            { id = newID
                            , name = containerName
                            , position = newPos
                            , rect = (100, 100)
                            , amount = 0
                            , capacity = 0
                            }

                        model'' =
                          { model'
                          | containerByID = Dict.insert newContainer.id newContainer containerByID
                          }
                        pairs : List (Container, Jack)
                        pairs = List.map ((,) newContainer) jacks

                        model''' = List.foldl
                          addLink
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
      let
        model' = Calc.updateContainers model
      in
        { model | epoch = epoch }

    Tick t ->
      let
        u _ link = { link | textOffset = link.textOffset + textOffsetRate }
        linkByID' = linkByID |> Dict.map u
      in
        { model | linkByID = linkByID' }


addLink' : (Container, Jack) -> Model -> (Model, Link)
addLink' (container, jack) model =
  let
    --id = nextID model.linkByID
    link =
      { id = container.id + jack.id
      , containerID = container.id
      , jackID = jack.id
      , textOffset = -2000
      }
    linkByID' = Dict.insert link.id link model.linkByID
    model' = { model | linkByID = linkByID' }
  in
    (model', link)

addLink : (Container, Jack) -> Model -> Model
addLink pair model = fst <| addLink' pair model

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
