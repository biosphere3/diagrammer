module Model exposing (..)

import Dict exposing (Dict)
import Focus exposing (..)
import Math.Vector2 exposing (..)
import Mouse

import Foci exposing (..)
import Util exposing (..)

type alias Model =
  { processByID : ProcessDict
  , jackByID : JackDict
  , containerByID : ContainerDict
  , flowByID : FlowDict

  -- ui
  , drag : Maybe Drag
  , globalTransform : Transform
  }

type alias Transform = { translate : Vec2, scale : Float }

type alias ID = Int

type alias ProcessDict = Dict ID Process
type alias JackDict = Dict ID Jack
type alias FlowDict = Dict ID Flow
type alias ContainerDict = Dict ID Container

type alias Process =
    { id : ID
    , name : String
    , description : String
    , imageURL : Maybe String
    , position : Vec2
    , rect : Rect
    }

type alias Flow =
  { id : ID
  , containerID : ID
  , jackID : ID
  --, direction : FlowDirection
  --, via : Resource
  , textOffset : Float
  }

type alias Container =
  { id : ID
  , name : String
  , position : Vec2
  , rect : Rect
  }

type alias Resource =
  { name : String
  , state : MatterState
  }

type alias Jack =
  { id : ID
  , name : String
  , processID : ID
  , rate : Float
  , direction : JackDirection
  , position : Vec2
  , rect : Rect
  }

type alias Physical a = { a | rect : Rect , position : Vec2 }

type alias Rect = (Float, Float)

type alias Drag =
  { start : Mouse.Position
  , current : Mouse.Position
  , target : Draggable
  }

type Draggable = DragProcess Process
               | DragJack Jack
               | DragContainer Container
               | DragScreen

type MatterState = Solid | Liquid | Gas | Plasma

type JackDirection = Input | Output
type FlowDirection = InFlow | OutFlow

getProcessPosition : Model -> Process -> Vec2
getProcessPosition {drag} {id, position} =
    case drag of
      Nothing ->
        position

      Just drag ->
        case drag.target of
          DragProcess dragProcess ->
            if dragProcess.id == id
            then
              position `add` dragOffset drag
            else
              position
          _ -> position


getContainerPosition : Model -> Container -> Vec2
getContainerPosition {drag} {id, position} =
    case drag of
      Nothing ->
        position

      Just drag ->
        case drag.target of
          DragContainer dragContainer ->
            if dragContainer.id == id
            then
              position `add` dragOffset drag
            else
              position
          _ -> position


getJackProcess : Model -> Jack -> Process
getJackProcess model jack =
  seize jack.processID model.processByID


getJackPosition : Model -> Jack -> Vec2
getJackPosition {drag, processByID} {id, position, processID} =
  let
    process = seize processID processByID
  in
    case drag of
      Nothing ->
        position

      Just ({start, current, target} as drag) ->
        let offset = dragOffset drag
        in
          case target of
            DragProcess dragProcess ->
              if dragProcess.id == process.id
              then position `add` offset
              else position
            DragJack dragJack ->
              if dragJack.id == id
              then position `add` offset
              else position
            _ -> position


getGlobalTransform : Model -> Transform
getGlobalTransform (model) =
  case model.drag of
    Nothing -> model.globalTransform
    Just ({target} as drag) ->
      case target of
        DragScreen ->
          update (globalTransform => translate) (add <| dragOffset drag) model
          |> .globalTransform
        _ -> model.globalTransform



-- helpers ------------------------------------------

--dragOffset : { a | current : Mouse.Position, start : Mouse.Position } -> Vec2
dragOffset {current, start} = vec2 (toFloat <| current.x - start.x) (toFloat <| current.y - start.y)

