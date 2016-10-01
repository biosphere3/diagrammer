module Model exposing (..)

import Dict exposing (Dict)
import Mouse exposing (Position)

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
  { start : Position
  , current : Position
  , target : Draggable
  }

type alias ProcessDict = Dict ID Process
type alias JackDict = Dict ID Jack
type alias FlowDict = Dict ID Flow
type alias ContainerDict = Dict ID Container

--type alias Positioned a =
--  { a | position : Position
--  }

jackRadius = 20

type alias Process =
    { id : ID
    , name : String
    , description : String
    , imageURL : Maybe String
    , position : Position
    , shape : Shape
    }

type alias Flow =
  { id : ID
  , containerID : ID
  , jackID : ID
  , direction : FlowDirection
  --, via : Resource
  }

type alias Container =
  { id : ID
  , name : String
  , position : Position
  , shape : Shape
  }

type alias Resource =
  { name : String
  , state : MatterState
  }

type alias Physical a = { a | shape : Shape , position : Position }

type Shape = Rect Int Int
           | Chevron Int Int
           | Circle Int

type Draggable = DragProcess Process | DragJack Jack | DragContainer Container

type MatterState = Solid | Liquid | Gas | Plasma

type JackDirection = Input | Output
type FlowDirection = InFlow | OutFlow

type alias Jack =
  { id : ID
  , name : String
  , processID : ID
  , rate : Float
  , direction : JackDirection
  , position : Position
  , shape : Shape
  }

getProcessPosition : Model -> Process -> Position
getProcessPosition {drag} {id, position} =
    case drag of
      Nothing ->
        position

      Just {start, current, target} ->
        case target of
          DragProcess dragProcess ->
            if dragProcess.id == id
            then
              Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)
            else
              position
          _ -> position


getContainerPosition : Model -> Container -> Position
getContainerPosition {drag} {id, position} =
    case drag of
      Nothing ->
        position

      Just {start, current, target} ->
        case target of
          DragContainer dragContainer ->
            if dragContainer.id == id
            then
              Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)
            else
              position
          _ -> position


getJackPosition : Model -> Jack -> Position
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
              then position /+/ offset
              else position
            DragJack dragJack ->
              if dragJack.id == id
              then position /+/ offset
              else position
            _ -> position


-- helpers ------------------------------------------

dragOffset {current, start} =
  Position
    (current.x - start.x)
    (current.y - start.y)

