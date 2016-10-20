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

    | DragEndTargetJack Jack
    | DragEndTargetContainer Container

    | MouseWheelTurn Mouse.Position (Int, Int) Float

    | SetEpoch Int

    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( manageCache msg <| updateHelp msg <| model
  , Cmd.none
  )


{-| Populate the calcCache appropriately based on the Msg
Note that next time getCalc is called, it will just be a Dict lookup
-}
manageCache : Msg -> Model -> Model
manageCache msg model =
  let
    calc model =
      { model | calcCache = snd <| Calc.getCalc model }
    clear model =
      { model | calcCache = Dict.empty }
    recalc = calc << clear
  in case msg of
    DragStart _ _             -> model
    DragAt _                  -> model
    MouseWheelTurn _ _ _      -> model
    Tick _                    -> model
    SetEpoch _                -> calc model
    DragEnd _                 -> recalc model
    DragEndTargetJack _       -> recalc model
    DragEndTargetContainer _  -> recalc model


connectJacks model dragJack jack =
  let
    jacks = [dragJack, jack]
    newContainer =
      let
        newPos = centroid <| List.map .position jacks
        containerName = dragJack.name
      in
        { id = generateContainerID containerName
        , name = containerName
        , position = newPos
        , radius = 62
        , capacity = 1/0
        , initialAmount = 0
        }

    model' =
      { model
      | containerByID = Dict.insert newContainer.id newContainer model.containerByID
      }
    pairs : List (Container, Jack)
    pairs = List.map ((,) newContainer) jacks

    model'' = List.foldl
      addLink
      model'
      pairs
  in
    model''

updateHelp : Msg -> Model -> Model
updateHelp msg ({processByID, jackByID, containerByID, linkByID, drag} as model) =
  let
    {getProcess, getJack, getContainer, getLink} = getGetters model
  in case msg of

    DragStart target xy ->
      { model | drag = (Just (Drag xy xy target)) }


    DragAt xy ->
      { model | drag = (Maybe.map (\{start, target} -> Drag start xy target) model.drag) }


    DragEndTargetJack jack ->
      case drag of
        Just drag ->
          case drag.target of
            DragJack dragJack ->
              connectJacks model dragJack jack
            _ ->
              model
        Nothing ->
          model


    DragEndTargetContainer container ->
      case drag of
        Just drag ->
          case drag.target of
            DragJack dragJack ->
              addLink (container, dragJack) model
            _ ->
              model
        Nothing ->
          model


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
            DragJack dragJack -> model'


    MouseWheelTurn position (width, height) delta ->
      let
        ratio = Debug.log "delta" <| 1 + delta / 100
      in
        Focus.update globalTransform (\xf -> {xf | scale = xf.scale * ratio}) model


    SetEpoch epoch ->
      { model
      | epoch = epoch
      }


    Tick t ->
      let
        rate jack = case jack.direction of
          Input -> -textOffsetRate
          Output -> textOffsetRate
        u _ link =
          let
            offsetRate = Dict.get link.jackID model.jackByID |> Maybe.map rate |> Maybe.withDefault 0
          in
            { link | textOffset = link.textOffset + offsetRate }
        linkByID' = linkByID |> Dict.map u
      in
        { model | linkByID = linkByID' }


addLink' : (Container, Jack) -> Model -> (Model, Link)
addLink' (container, jack) model =
  let
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


getJacks : Model -> Process -> List Jack
getJacks {jackByID} process =
  jackByID
  |> Dict.values
  |> List.filter (\x -> x.processID == process.id)


joinJacks : Model -> List Jack -> Model
joinJacks model jacks = model
  -- add a link between these jacks
