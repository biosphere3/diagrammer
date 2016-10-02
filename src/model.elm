module Model exposing (..)

import Dict exposing (Dict)
import Math.Vector2 exposing (..)
import Mouse

import Util exposing (..)

type alias Model =
  { processByID : ProcessDict
  , jackByID : JackDict
  , containerByID : ContainerDict
  , flowByID : FlowDict

  -- ui
  , drag : Maybe Drag
  }


type alias ID = Int

type alias Drag =
  { start : Mouse.Position
  , current : Mouse.Position
  , target : Draggable
  }

type alias ProcessDict = Dict ID Process
type alias JackDict = Dict ID Jack
type alias FlowDict = Dict ID Flow
type alias ContainerDict = Dict ID Container

--type alias Positioned a =
--  { a | position : Vec2
--  }

jackRadius = 20

type alias Process =
    { id : ID
    , name : String
    , description : String
    , imageURL : Maybe String
    , position : Vec2
    , shape : Shape
    }

type alias Flow =
  { id : ID
  , containerID : ID
  , jackID : ID
  --, direction : FlowDirection
  --, via : Resource
  }

type alias Container =
  { id : ID
  , name : String
  , position : Vec2
  , shape : Shape
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
  , shape : Shape
  }

type alias Physical a = { a | shape : Shape , position : Vec2 }

type Shape = Rect Float Float
           | Chevron Float Float
           | Circle Float

type Draggable = DragProcess Process | DragJack Jack | DragContainer Container

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


-- helpers ------------------------------------------

--dragOffset : { a | current : Mouse.Position, start : Mouse.Position } -> Vec2
dragOffset {current, start} = vec2 (toFloat <| current.x - start.x) (toFloat <| current.y - start.y)

